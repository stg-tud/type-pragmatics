package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.FreshNames

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
        Seq(dt, finit, fenum, Axioms(Seq(enumEq)))
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
}

