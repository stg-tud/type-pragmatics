package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.veritas.ExistsJudgment
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.MetaVar
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.TypingRule
import de.tu_darmstadt.veritas.backend.veritas.TypingRuleJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.veritas.DataType
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionDef
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionSig
import de.tu_darmstadt.veritas.backend.veritas.SortDef
import de.tu_darmstadt.veritas.backend.veritas.SortRef
import de.tu_darmstadt.veritas.backend.veritas.Functions
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpVar

/**
 * inserts a ground guards definition that checks if its argument is a ground term, only constructed from
 *  - data-type constructors (by the axiom generated here)
 *  - open-type constants (by the premises on constants generated in the execution goal)
 *
 * inserts ground guards requirements for existentially quantified variables in some goals
 */
object GenerateGroundGuards extends ModuleTransformation with CollectTypes {
  def groundName(typ: String) = s"groundGuard$typ"
  
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = 
    withSuper(super.transModuleDefs(mdef)) {
      case dt@DataType(dopen, dname, constrs) =>
        val groundFun = FunctionDef(FunctionSig(groundName(dname), Seq(SortRef(dname)), SortRef("Bool")), Seq())
        var rules = Seq[TypingRule]()
        for (DataTypeConstructor(cname, in) <- constrs) {
          val fresh = new FreshNames
          val cargs = in.map(sort => FunctionMeta(MetaVar(fresh.freshName(sort.name))))
          val ccall = FunctionExpApp(cname, cargs)
          val conclusion = FunctionExpJudgment(FunctionExpApp(groundName(dname), Seq(ccall)))
          val premises = in.zip(cargs).map{case (typ, exp) => 
            FunctionExpJudgment(FunctionExpApp(groundName(typ.name), Seq(exp)))}
          rules :+= TypingRule(s"ground-$dname-$cname", premises, Seq(conclusion))
        }
        if (dopen)
          Seq(dt, Functions(Seq(groundFun)))
        else
          Seq(dt, Functions(Seq(groundFun)), Axioms(rules))
  
      case goal@Goals(gs, t) =>
        val constAxioms = consts.toSeq.map { c =>
          val typ = constrTypes(c)._2
          val ref = FunctionExpApp(c, Seq())
          val conclusion = FunctionExpJudgment(FunctionExpApp(groundName(typ.name), Seq(ref)))
          TypingRule(s"ground-$c", Seq(), Seq(conclusion))
        }
        Seq(Axioms(constAxioms), goal)
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = trj match {
    case ex@ExistsJudgment(vl, jl) if shouldMakeGroundRequirements(ex) =>
      val vars = FreeVariables.apply(jl).filter(v => vl.exists(_.name == v.name))
      val types = inferMetavarTypes(vars, jl)
      // every existentially quantified variable needs to have a ground witness
      val groundRequirements = vars map (v => 
        FunctionExpJudgment(FunctionExpApp(groundName(types(v).name), Seq(FunctionMeta(v)))))
      Seq(ExistsJudgment(vl, jl ++ groundRequirements))
    case _ => super.transTypingRuleJudgments(trj)
  }
  
  def shouldMakeGroundRequirements(ex: ExistsJudgment): Boolean = {
    var isGoal = false
    var isRelevant = false
    for (vc <- path) {
      if (vc.isInstanceOf[Goals])
        isGoal = true
      else if (vc.isInstanceOf[TypingRule]) {
        val tr = vc.asInstanceOf[TypingRule]
        isRelevant = isRelevant || tr.name.startsWith("execution") || tr.name.startsWith("counterexample") || tr.name.startsWith("synthesis")
      }
    }
    isGoal && isRelevant
  }
}

