package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.ast.function._

/**
 * For each Constructors() node, it generates a following Axioms() node, containing the EQ and DIFF
 * axioms for each ConstructorDecl inside the Constructors(). 
 * 
 * Also works with Local/Strategy blocks. No longer (!) assumes one single Constructors() node per 
 * Module, multiple are fine now...
 */
object GenerateDiffAxiomsForConsts extends ModuleTransformation {
  
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case c@Consts(cs, true) =>
        val diffAxioms = makeDiffAxioms(cs)
        Seq(c, Axioms(diffAxioms))
    }
  }

  private def makeDiffAxioms(constrs: Seq[ConstDecl]) = 
    for (i <- 0 until constrs.size;
         j <- i + 1 until constrs.size 
           if constrs(i).out == constrs(j).out) // avoid diff axioms for consts of incompatible types
      yield makeDiffAxiom(constrs(i), constrs(j))
  
  
  private def makeDiffAxiom(c1: ConstDecl, c2: ConstDecl) = {
    val freshNames = new FreshNames
    val argsLeft = Seq()
    val argsRight = Seq()

    TypingRule("DIFF-" + c1.name + "-" + c2.name, Nil,
      Seq(FunctionExpNeq(FunctionExpApp(c1.name, argsLeft),
        FunctionExpApp(c2.name, argsRight))))
  }
}