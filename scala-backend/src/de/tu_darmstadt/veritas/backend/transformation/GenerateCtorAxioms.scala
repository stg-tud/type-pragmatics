package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.Constructors
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpAnd
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpApp
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpBiImpl
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpEq
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment.wrap
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.veritas.MetaVar
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.TypingRule

/**
 * Precondition: The module has only a single Constructors()
 */
object GenerateCtorAxioms extends ModuleDefTransformation {
  override protected def checkPrecondition(input: Module): Unit = {
    if (input.body.filter(_.isInstanceOf[Constructors]).size > 1)
      throw TransformationError("Expected a Module() with a single Constructors() ModuleDef, got: " + input)
  }
  
  override protected def apply: PartialFunction[ModuleDef, Seq[ModuleDef]] = {
    case input@Constructors(decls) => {
      // generate EQ axioms
      var generatedAxioms = decls map { constructor =>
        val args = constructor.in.map(_.name)
        
        val freshNames = new FreshNames
        val argsLeft = freshNames(args) map (MetaVar(_)) map (FunctionExpMeta(_))
        val argsRight = freshNames(args) map (MetaVar(_)) map (FunctionExpMeta(_))
        
        // no premise, just a biimplication as conclusion
        TypingRule("EQ-" + constructor.name, Nil, 
          Seq(FunctionExpBiImpl(
            FunctionExpAnd((argsLeft, argsRight).zipped map (FunctionExpEq(_, _))),
            FunctionExpEq(FunctionExpApp(constructor.name, argsLeft),
                          FunctionExpApp(constructor.name, argsRight))
          )))
      }
      
      // generate DIFF axioms
      // TODO
      
      Seq(input, Axioms(generatedAxioms))
    }
  }
}