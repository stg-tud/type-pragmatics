package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes



case class CurrentFun(
   // saves the signature of the function for which an inversion axiom is to be generated
   name: String, params: Seq[SortRef], result: SortRef,
   // saves variable names for premise
   genParamNames: Seq[MetaVar], genResultName: MetaVar)

/**
 * generates inversion axioms for functions and partial functions
 * assumes that function equations have already been transformed to axioms!
 * assumes that all equations for one function are contained in a single Axioms-block
 * (uses the discovered function axioms for generating the inversion lemmas!)
 *
 */
trait FunctionInversionAxioms extends CollectTypes {
  /**
   * override to control for which functions inversion axioms are generated!
   * default: all! (even partial functions)
   *
   * parameter: name of the function (overriding classes can use functypes/pfunctypes to check what function it is!)
   */
  def checkFunction(fn: String): Boolean = true

  /**
   * override to control the names of variables in premises
   * default: sort names for function arguments, result for function result
   *
   * parameter: none, uses currfs to generate names (sets genNames)
   */
  def newMetaVars(params: Seq[SortRef]): (Seq[MetaVar], MetaVar) = {
    val freshNames = new FreshNames
    (for (sr <- params) yield MetaVar(freshNames.freshName(sr.name)), MetaVar("RESULT"))
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case as @ Axioms(tseq) =>
        isFunctionDef(tseq) match {
          case None => Seq(as)
          case Some(current) => Seq(as, generateInversionAxiom(current, tseq))
        }
    }

  /**
   * assumption: if all axioms in a block start with a function name, then these axioms belong to
   * the same function definition
   */
  private def isFunctionDef(tseq: Seq[TypingRule]): Option[CurrentFun] = {
    val rulenames = for (tr <- tseq) yield tr match {
      case TypingRule(n, _, _) => n
    }

    def nameMatches(rn: String, fn: String): Boolean = {
      val eqnameregex = """([a-zA-Z0-9]+)\-?[0-9]+""".r.unanchored
      rn match {
        case eqnameregex(fnname) => (fnname == fn)
        case _                   => false
      }
    }

    if (!rulenames.isEmpty) {
      val func: Seq[String] = ((for { fn <- functypes.keys if (rulenames.forall { rn => nameMatches(rn, fn) }) } yield fn) ++
        (for { fn <- pfunctypes.keys if (rulenames.forall { rn => nameMatches(rn, fn) }) } yield fn)).toSeq

      func match {
        case Seq()                          => None
        case Seq(fn) if (checkFunction(fn)) =>
          val (params, res) = functypes.getOrElse(fn, pfunctypes(fn))
          val (genParamNames, genResultName) = newMetaVars(params)
          Some(CurrentFun(fn, params, res, genParamNames, genResultName))
        case _                              => None
      }
    } else None
  }

  def generateInversionAxiom(currentFun: CurrentFun, tseq: Seq[TypingRule]): Axioms = {
    if (currentFun.result.name != "Bool") {
      val orcases = for (ts <- tseq) yield makeInvCase(currentFun, ts)
      val conc = if (orcases.size > 1) Seq(OrJudgment(orcases)) else orcases.head
      Axioms(Seq(TypingRule(currentFun.name + "-INV", Seq(makeInvFunPremise(currentFun)), conc)))
    } else {
      val truerules = filterTrue(currentFun, tseq)
      val truecases = for (ts <- truerules) yield makeInvTrueCase(currentFun, ts)
      val tconc = truecases.size match {
        case 0 => Seq()
        case 1 => truecases.head
        case _ => Seq(OrJudgment(truecases))
      }
      val trueRule = if(tconc.isEmpty) Seq() else Seq(TypingRule(currentFun.name + "-true-INV", Seq(makeInvTruePremise(currentFun)), tconc))
      val falserules = filterFalse(currentFun, tseq)
      val falsecases = for (ts <- falserules) yield makeInvFalseCase(currentFun, ts)
      val fconc = falsecases.size match {
        case 0 => Seq()
        case 1 => falsecases.head
        case _ => Seq(OrJudgment(falsecases))
      }
      val falseRule = if(fconc.isEmpty) Seq() else Seq(TypingRule(currentFun.name + "-false-INV", Seq(makeInvFalsePremise(currentFun)), fconc))
      Axioms(trueRule ++ falseRule)
    }
  }

  private def makeInvTruePremise(currentFun: CurrentFun): FunctionExpJudgment =
    FunctionExpJudgment(
      FunctionExpApp(currentFun.name, currentFun.genParamNames map { m => FunctionMeta(m) }))

  private def makeInvFalsePremise(currentFun: CurrentFun): FunctionExpJudgment =
    FunctionExpJudgment(
      FunctionExpNot(FunctionExpApp(currentFun.name, currentFun.genParamNames map { m => FunctionMeta(m) })))

  private def makeInvFunPremise(currentFun: CurrentFun): FunctionExpJudgment =
    FunctionExpJudgment(FunctionExpEq(
      FunctionExpApp(currentFun.name, currentFun.genParamNames map { m => FunctionMeta(m) }),
      FunctionMeta(currentFun.genResultName)))

  private def filterTrue(currentFun: CurrentFun, tseq: Seq[TypingRule]): Seq[TypingRule] =
    tseq filter {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpApp(fn, _)))) if (fn == currentFun.name) => true
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), _)))) if (fn == currentFun.name) => true
      case _ => false
    }

  private def filterFalse(currentFun: CurrentFun, tseq: Seq[TypingRule]): Seq[TypingRule] =
    tseq filter {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpNot(FunctionExpApp(fn, _))))) if (fn == currentFun.name) => true
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), _)))) if (fn == currentFun.name) => true
      case _ => false
    }

  private def makeInvCase(currentFun: CurrentFun, ts: TypingRule): Seq[TypingRuleJudgment] = {
    val (params, res) = ts match {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpEq(FunctionExpApp(fn, pars), r)))) if (fn == currentFun.name) => (pars, r)
      case _ => throw TransformationError("Generation of inversion axioms: Wrong shape of definition axiom")
    }
    val parambind = for ((n, p) <- currentFun.genParamNames zip params) yield FunctionExpJudgment(FunctionExpEq(FunctionMeta(n), p))
    val resbind = FunctionExpJudgment(FunctionExpEq(FunctionMeta(currentFun.genResultName), res))
    val existsbody = ts.premises ++ parambind :+ resbind
    val genNamesSet = ((currentFun.genParamNames) :+ currentFun.genResultName).toSet
    val freevars = FreeVariables.freeVariables(ts.premises, genNamesSet) ++
      FreeVariables.freeVariables(parambind, genNamesSet) ++
      FreeVariables.freeVariables(resbind, genNamesSet)
    if (freevars.isEmpty)
      existsbody
    else
      Seq(ExistsJudgment(freevars.toSeq, existsbody))
  }

  private def makeInvTrueCase(currentFun: CurrentFun, ts: TypingRule): Seq[TypingRuleJudgment] = {
    val params = ts match {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpApp(fn, pars)))) if (fn == currentFun.name) => pars
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, pars), _)))) if (fn == currentFun.name) => pars
      case _ => throw TransformationError("Generation of inversion axioms: Wrong shape of Boolean definition axiom")
    }
    val cond = ts match {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), c)))) if (fn == currentFun.name) => Some(c)
      case _ => None
    }
    val parambind = for ((n, p) <- currentFun.genParamNames zip params) yield FunctionExpJudgment(FunctionExpEq(FunctionMeta(n), p))
    val existsbody = if (cond == None) ts.premises ++ parambind else ts.premises ++
      parambind :+ FunctionExpJudgment(cond.get)
    val genNamesSet = currentFun.genParamNames.toSet
    val freevars = FreeVariables.freeVariables(ts.premises, genNamesSet) ++
      FreeVariables.freeVariables(parambind, genNamesSet) ++
      FreeVariables.freeVariables(cond.getOrElse(FunctionExpTrue), genNamesSet)
    if (freevars.isEmpty)
      existsbody
    else
      Seq(ExistsJudgment(freevars.toSeq, existsbody))
  }

  private def makeInvFalseCase(currentFun: CurrentFun, ts: TypingRule): Seq[TypingRuleJudgment] = {
    val params = ts match {
      //look for negated conclusion
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpNot(FunctionExpApp(fn, pars))))) if (fn == currentFun.name) => pars
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, pars), _)))) if (fn == currentFun.name) => pars
      case _ => throw TransformationError("Generation of inversion axioms: Wrong shape of Boolean definition axiom")
    }
    val cond = ts match {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), c)))) if (fn == currentFun.name) => Some(c)
      case _ => None
    }
    val parambind = for ((n, p) <- currentFun.genParamNames zip params) yield FunctionExpJudgment(FunctionExpEq(FunctionMeta(n), p))
    //negate condition
    val existsbody = if (cond == None) ts.premises ++ parambind else ts.premises ++
      parambind :+ FunctionExpJudgment(FunctionExpNot(cond.get))
    val genNamesSet = currentFun.genParamNames.toSet
    val freevars = FreeVariables.freeVariables(ts.premises, genNamesSet) ++
      FreeVariables.freeVariables(parambind, genNamesSet) ++
      FreeVariables.freeVariables(cond.getOrElse(FunctionExpTrue), genNamesSet)
    if (freevars.isEmpty)
      existsbody
    else
      Seq(ExistsJudgment(freevars.toSeq, existsbody))
  }

}

object AllFunctionInversionAxioms extends FunctionInversionAxioms

object TotalFunctionInversionAxioms extends FunctionInversionAxioms {
  override def checkFunction(fn: String): Boolean =
    !pfunctypes.isDefinedAt(fn)
}
