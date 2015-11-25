package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.Configuration

/**
 * For each SortDef we generate a type guard and for each ConstructorDecl we generate an axiom for the guard.
 *
 * cons D : T
 * ==>
 * axiom $T(D)
 *
 * cons E : T * U -> V
 * ==>
 * axiom $T(x), $U(y) <-> $V(E(x,y))
 *
 * Also works with Local/Strategy blocks.
 */
object GenerateTypeGuards extends ModuleTransformation {

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case dt@DataType(open, name, constrs) =>
        val guardAxioms = constrs map (makeGuardAxiom(_, name))
        if (open)
          Seq(dt, Axioms(guardAxioms))
        else {
          val domAxiom = makeDomainAxiom(name, constrs)
          Seq(dt, Axioms(guardAxioms :+ domAxiom))
        }
    }
  }

  private def guard(name: String): String = "guard-" + name
  
  private def guardCall(sort: String, arg: FunctionExpMeta): FunctionExpApp = 
    FunctionExpApp(guard(sort), Seq(arg))
  
  private def makeGuardAxiom(cd: DataTypeConstructor, dataType: String): TypingRule = {
    val fresh = new FreshNames
    val vars = cd.in.map(sort => FunctionMeta(MetaVar(fresh.freshName(sort.name))))
    
    // all vars are well-typed
    val premises = cd.in.zip(vars).map { case (sort, v) =>
      FunctionExpJudgment(guardCall(sort.name, v))
    }
    
    // the constructor call yields something well-typed
    val consCall = FunctionExpApp(cd.name, vars)
    val consequence = FunctionExpJudgment(guardCall(dataType, consCall))
    
    val name = s"guard-$dataType-${cd.name}"
    val rule = TypingRule(name, premises, Seq(consequence))
    rule
  }
  
  private def makeDomainAxiom(dataType: String, constrs: Seq[DataTypeConstructor]): TypingRule = {
    val name = s"guard-dom-$dataType"
    val v = FunctionMeta(MetaVar("X"))
    
    // all v. guard(v) => (v=c1(...) | ... | v=cn(...))
    // for n=0, simplifies to all v. not guard(v)
    TypingRule(
        name, 
        Seq(FunctionExpJudgment(guardCall(dataType, v))), 
        Seq(OrJudgment(constrs map (c => Seq(makeEqConsFormula(c, v))))))
  }
  
  private def makeEqConsFormula(cd: DataTypeConstructor, v: FunctionMeta): TypingRuleJudgment = {
    val fresh = new FreshNames
    val vars = cd.in.map(sort => MetaVar(fresh.freshName(sort.name)))
    
    val eq = FunctionExpEq(v, FunctionExpApp(cd.name, vars map (FunctionMeta(_))))
    ExistsJudgment(vars, Seq(FunctionExpJudgment(eq)))
  }
}