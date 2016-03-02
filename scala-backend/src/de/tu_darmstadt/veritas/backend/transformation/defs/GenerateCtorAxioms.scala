package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.function._

/**
 * For each Constructors() node, it generates a following Axioms() node, containing the EQ and DIFF
 * axioms for each ConstructorDecl inside the Constructors().
 *
 * Also works with Local/Strategy blocks. No longer (!) assumes one single Constructors() node per
 * Module, multiple are fine now...
 */
object GenerateCtorAxioms extends ModuleTransformation {

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case dt @ DataType(open, name, constrs) =>
        val domAxiom = if (!open) Seq(makeDomainAxiom(name, constrs)) else Seq()
        val eqAxioms = constrs map (makeEqAxiom(_))
        val diffAxioms = makeDiffAxioms(constrs)
        Seq(dt, Axioms(domAxiom ++ eqAxioms ++ diffAxioms))
    }
  }
  
  private def makeDomainAxiom(dataType: String, constrs: Seq[DataTypeConstructor]): TypingRule = {
    val name = s"dom-$dataType"
    val v = FunctionMeta(MetaVar("X"))

    // all v. guard(v) => (guard(c1_i)&v=c1(c1_1...c1_k) | ... | guard(cn_i)&v=cn(cn_1...cn_k))
    // for n=0, simplifies to all v. not guard(v)
    TypingRule(
      name,
      Seq(),
      Seq(OrJudgment(constrs map (c => Seq(makeEqConsFormula(c, v))))))
  }
  
  def makeEqConsFormula(cd: DataTypeConstructor, v: FunctionMeta): TypingRuleJudgment = {
    val fresh = new FreshNames
    val vars = cd.in.map(sort => MetaVar(fresh.freshName(sort.name)))

    val eq = FunctionExpEq(v, FunctionExpApp(cd.name, vars map (FunctionMeta(_))))

    ExistsJudgment(vars, Seq(FunctionExpJudgment(eq)))
  }

  private def makeEqAxiom(c: DataTypeConstructor) = {
    val freshNames = new FreshNames
    val args = c.in map (_.name)
    val argsLeft = freshNames(args) map (x => FunctionMeta(MetaVar(x)))
    val argsRight = freshNames(args) map (x => FunctionMeta(MetaVar(x)))

    // needed implication only in one direction, the other is satisfied for function calls anyway
    TypingRule("EQ-" + c.name,
      Seq(FunctionExpJudgment(
        FunctionExpEq(FunctionExpApp(c.name, argsLeft),
          FunctionExpApp(c.name, argsRight)))),
      Seq(FunctionExpJudgment(
        FunctionExpAnd((argsLeft, argsRight).zipped map (FunctionExpEq(_, _))))))
  }

  private def makeDiffAxioms(constrs: Seq[DataTypeConstructor]) =
    for (
      i <- 0 until constrs.size;
      j <- i + 1 until constrs.size
    ) yield makeDiffAxiom(constrs(i), constrs(j))

  private def makeDiffAxiom(c1: DataTypeConstructor, c2: DataTypeConstructor) = {
    val freshNames = new FreshNames
    val args1 = c1.in map (_.name)
    val args2 = c2.in map (_.name)
    val argsLeft = freshNames(args1) map (x => FunctionMeta(MetaVar(x)))
    val argsRight = freshNames(args2) map (x => FunctionMeta(MetaVar(x)))

    TypingRule("DIFF-" + c1.name + "-" + c2.name, Nil,
      Seq(FunctionExpNeq(FunctionExpApp(c1.name, argsLeft),
        FunctionExpApp(c2.name, argsRight))))
  }
}