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
 * inserts a ground guards that checks if its argument is a ground term, only constructed from
 *  - data-type constructors (by the axiom generated here)
 *  - open-type constants (by the premises on constants generated in the execution goal)
 */
object GenerateGroundGuard extends ModuleTransformation {
  def groundName(typ: String) = s"groundGuard$typ"

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case DataType(dopen, dname, constrs) =>
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
        Seq(mdef, Functions(Seq(groundFun)))
      else
        Seq(mdef, Functions(Seq(groundFun)), Axioms(rules))
    case _ => Seq(mdef)
  }
}

/**
 * inserts lightweight type guards (i.e. domain axiom only!) for existentially quantified variables
 * in execution goals
 */
object InsertGroundGuardsInExecutionGoals extends ModuleTransformation with CollectTypes {
  
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case Goals(gs, t) =>
      val constAxioms = consts.toSeq.map { c =>
        val typ = constrTypes(c)._2
        val ref = FunctionExpApp(c, Seq())
        val conclusion = FunctionExpJudgment(FunctionExpApp(GenerateGroundGuard.groundName(typ.name), Seq(ref)))
        TypingRule(s"ground-$c", Seq(), Seq(conclusion))
      }
      Axioms(constAxioms) +: super.transModuleDefs(mdef)
    case _ => super.transModuleDefs(mdef)
  }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = trj match {
    case ex@ExistsJudgment(vl, jl) if shouldMakeGroundRequirements(ex) =>
      val vars = FreeVariables.apply(jl).filter(v => vl.exists(_.name == v.name))
      val types = inferMetavarTypes(vars, jl)
      // every existentially quantified variable needs to have a ground witness
      val groundRequirements = vars map (v => 
        FunctionExpJudgment(FunctionExpApp(GenerateGroundGuard.groundName(types(v).name), Seq(FunctionMeta(v)))))
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
        isRelevant = isRelevant || tr.name.startsWith("execution")
      }
    }
    isGoal && isRelevant
  }
}

