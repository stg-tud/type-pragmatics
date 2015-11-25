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
      case dt@DataType(open, name, constrs) =>
        val eqAxioms = constrs map (makeEqAxiom(_))
        if (open)
          Seq(dt, Axioms(eqAxioms))
        else {
          val diffAxioms = makeDiffAxioms(constrs)
          Seq(dt, Axioms(eqAxioms ++ diffAxioms))
        }
    }
  }

  private def makeEqAxiom(c: DataTypeConstructor) = {
    val freshNames = new FreshNames
    val args = c.in map (_.name)
    val argsLeft = freshNames(args) map (x => FunctionMeta(MetaVar(x)))
    val argsRight = freshNames(args) map (x => FunctionMeta(MetaVar(x)))

    // no premise, just a biimplication as conclusion
    TypingRule("EQ-" + c.name, Nil,
      Seq(FunctionExpBiImpl(
        FunctionExpAnd((argsLeft, argsRight).zipped map (FunctionExpEq(_, _))),
        FunctionExpEq(FunctionExpApp(c.name, argsLeft),
          FunctionExpApp(c.name, argsRight)))))
  }

  
  private def makeDiffAxioms(constrs: Seq[DataTypeConstructor]) = 
    for (i <- 0 until constrs.size;
         j <- i + 1 until constrs.size)
      yield makeDiffAxiom(constrs(i), constrs(j))
  
  
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