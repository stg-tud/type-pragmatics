package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesDefs
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.ast.ExistsJudgment
import de.tu_darmstadt.veritas.backend.ast.ForallJudgment
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.NotJudgment
import de.tu_darmstadt.veritas.backend.ast.OrJudgment
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.ast.TypingRuleJudgment
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpEq
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta

/**
 * transformations for modules right before they are translated to barefof
 * (where all types are erased)
 *
 * when encountering a free meta variable, checks whether type of variable has
 * a finite domain - if yes, quantification over variable is rewritten as follows:
 *
 * assume T = c1(T1) | ... | cn(Tn)
 * forall x: T. phi ---> Conjunction over all constructors i: forall yi. x = ci(yi) => phi
 * exists x: T. phi ---> Disjunction over all constructors i: exists yi. x = ci(yi) & phi
 */
object RewriteFiniteDomainQuantifications extends ModuleTransformation with CollectTypesDefs {

  // contains types which were already checked to have a finite domain
  private var finiteTypeCache: Set[SortRef] = Set()
  // contains types which were already checked to have an infinite domain
  private var infiniteTypeCache: Set[SortRef] = Set()

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    finiteTypeCache = Set()
    infiniteTypeCache = Set()
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    inferMetavarTypes(tr)
    tr match {
      case TypingRule(n, prems, conss) => {
        val newprems = trace(prems)(transTypingRuleJudgments(_))
        val newconss = trace(conss)(transTypingRuleJudgments(_))
        val additionalPremises = makeDomainPremises(collectCandidateVars(newprems ++ newconss))
        Seq(TypingRule(n, additionalPremises ++ newprems, newconss))
      }
    }
  }

  //cannot override transTypingRuleJudgment here??  

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case ExistsJudgment(vl, jl) => {
        val additionalPremises = makeDomainPremises(collectCandidateVars(jl))
        Seq(ExistsJudgment(vl, additionalPremises ++ jl))
      }
      case ForallJudgment(vl, jl) => {
        val additionalPremises = makeDomainPremises(collectCandidateVars(jl))
        Seq(ForallJudgment(vl, makeOrImpl(additionalPremises, jl)))
      }
    }

  private def makeOrImpl(prems: Seq[TypingRuleJudgment], cons: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    val premisesseq = prems map (p => NotJudgment(p)) map (s => Seq(s))
    val orcases = premisesseq :+ cons
    if (orcases.length > 1)
      Seq(OrJudgment(orcases))
    else
      orcases.head
  }

  //select variables with finite types (types of all metavars should already
  //be inferred when this method is called
  private def collectCandidateVars(trjseq: Seq[TypingRuleJudgment]): Set[MetaVar] =
    FreeVariables.freeVariables(trjseq) filter (mv => checkFinite(mv.sortType))

  private def makeEqConsFormula(cd: DataTypeConstructor, v: FunctionMeta): TypingRuleJudgment = {
    val fresh = new FreshNames
    val vars = cd.in.map(sort => MetaVar(fresh.freshName(sort.name)))

    val eq = FunctionExpEq(v, FunctionExpApp(cd.name, vars map (FunctionMeta(_))))

    ExistsJudgment(vars, Seq(FunctionExpJudgment(eq)))
  }

  private def makeDomainPremises(mvset: Set[MetaVar]): Seq[TypingRuleJudgment] =
    for (mv <- mvset.toSeq if mv.sortType.name != "Bool") yield {
      val constrs = dataTypes(mv.sortType.name)._2
      OrJudgment(constrs map (c => Seq(makeEqConsFormula(c, FunctionMeta(mv)))))
    }

  // true if sort refers to open data type
  private def checkOpen(s: SortRef): Boolean =
    if (s.name == "Bool")
      false
    else
      dataTypes(s.name)._1

  // true if sort either refers to itself (recursive) or to another type marked infinite
  private def checkInfinite(s: SortRef): Boolean = {
    //computes closure of references
    def refersTo(srset: Set[SortRef], seen: Set[SortRef]): Set[SortRef] =
      (for (sr <- srset if (!(seen contains sr))) yield dataTypes.get(sr.name) match {
        case None             => srset //this should not happen - generate an error?
        case Some((b, Seq())) => srset
        case Some((b, dcseq)) => srset union (for (dc <- dcseq) yield refersTo(dc.in.toSet, srset union dc.in.toSet)).flatten.toSet
      }).flatten

    if (s.name == "Bool")
      return false
      
    val references = dataTypes(s.name)._2
    references exists (dc =>
      refersTo(dc.in.toSet, Set()) exists (sr => (sr == s || !checkFinite(sr))))
  }

  /*
   * returns true if given SortRef refers to a finite type
   * updates finiteTypeCache as well
   */
  private def checkFinite(s: SortRef): Boolean = {
    // first check whether type was already checked to be finite/infinite
    if (finiteTypeCache contains s) true
    else if (infiniteTypeCache contains s) false
    //if unknown, determine and update caches accordingly
    else {
      if (checkOpen(s) || checkInfinite(s)) {
        infiniteTypeCache = infiniteTypeCache + s
        false
      } else {
        finiteTypeCache = finiteTypeCache + s
        true
      }
    }
  }

}