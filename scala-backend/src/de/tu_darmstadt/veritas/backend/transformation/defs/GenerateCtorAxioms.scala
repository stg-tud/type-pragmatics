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
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
      withSuper(super.transModule(name, is, mdefs)) {
        case mod: Module => Seq(Module(mod.name, mod.imports, generateEqAndDiffAxioms(mod.defs, Seq())))
      }
  
  private def generateEqAndDiffAxioms(defs: Seq[ModuleDef], _ctorsSoFar: Seq[ConstructorDecl]): Seq[ModuleDef] = {
    var ctorsSoFar = _ctorsSoFar
    defs.flatMap {
      // handle Constructors further down the ModuleDef tree
      case Local(localDefs) => Seq(Local(generateEqAndDiffAxioms(localDefs, ctorsSoFar)))
      case Strategy(name, imports, strategyDefs) => Seq(Strategy(name, imports, generateEqAndDiffAxioms(strategyDefs, ctorsSoFar)))
      
      // foreach Constructors node you see, generate a following Axioms() node, containing EQ and DIFF axioms
      case ctors @ Constructors(decls) if decls.nonEmpty => {
        val generatedAxioms = decls.flatMap { decl => 
          val args = decl.in map (_.name)
          
          /*
           * generate one EQ axiom per ConstructorDecl
           */
          val eqAxiom = {
            val freshNames = new FreshNames
            val argsLeft = freshNames(args) map (MetaVar(_)) map (FunctionMeta(_))
            val argsRight = freshNames(args) map (MetaVar(_)) map (FunctionMeta(_))
  
            // no premise, just a biimplication as conclusion
            TypingRule("EQ-" + decl.name, Nil,
              Seq(FunctionExpBiImpl(
                FunctionExpAnd((argsLeft, argsRight).zipped map (FunctionExpEq(_, _))),
                FunctionExpEq(FunctionExpApp(decl.name, argsLeft),
                  FunctionExpApp(decl.name, argsRight)))))
          }
          
          /*
           * and one DIFF axiom PER other ConstructorDecl seen so far
           * but only for those that have the same return type 
           * (otherwise, they aren't equal anyway, no need for a DIFF axiom) 
           */
          val diffAxioms = ctorsSoFar.filter(_.out == decl.out).map { otherDecl =>
            val freshNames = new FreshNames
            val argsLeft = freshNames(args) map (MetaVar(_)) map (FunctionMeta(_))
            val argsRight = freshNames(otherDecl.in map (_.name)) map (MetaVar(_)) map (FunctionMeta(_))
  
            TypingRule("DIFF-" + decl.name + "-" + otherDecl.name, Nil,
              Seq(FunctionExpNeq(FunctionExpApp(decl.name, argsLeft),
                FunctionExpApp(otherDecl.name, argsRight))))
          }

          // add the just processed ConstructorDecl to our "seen so far" list
          ctorsSoFar +:= decl

          eqAxiom +: diffAxioms
        }
        Seq(ctors, Axioms(generatedAxioms))        
      }

      case other => Seq(other)
    }
  }
}