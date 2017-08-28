package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment._

/**
  * Generates functions which encode that an open datatype is isomorph to the natural numbers
  * meaning that it is countably inifite
  */
object GenerateCountablyInfiniteEncoding extends ModuleTransformation {
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case dt@DataType(true, name, constrs) =>
        val t = SortRef(name)
        val initName = "init" + name
        val enumName = "enum" + name
        val finit = Functions(Seq(FunctionDef(FunctionSig(initName, Seq(), t), Seq())))
        val fenum = Functions(Seq(FunctionDef(FunctionSig(enumName, Seq(t), t), Seq())))
        val enumEq = makeEqAxiom(DataTypeConstructor(enumName, Seq(t)))
        val diff = makeDiffAxiom(DataTypeConstructor(initName, Seq()), DataTypeConstructor(enumName, Seq(t)))
        Seq(dt, finit, fenum, Axioms(Seq(enumEq, diff)))
    }
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

