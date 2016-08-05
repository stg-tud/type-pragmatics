package de.tu_darmstadt.veritas.backend.transformation.imports

import de.tu_darmstadt.veritas.backend.transformation.FilterGoalsAndLocals
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.ast.Import
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.Resolved
import de.tu_darmstadt.veritas.backend.ast.Strategy
import de.tu_darmstadt.veritas.backend.Configuration

/**
 * Replaces each import with all ModuleDefs in the imported Module (but filters out Goals and Local 
 * blocks). Does this also with imports in Strategy blocks.
 * 
 * The result of this transformation is a Module with empty imports list and no imports in any
 * strategy blocks.
 * 
 * You first must _resolve_ the imports, that is find the Module code of the referenced module by
 * the ResolveImports transformation.
 */
object ReplaceImportsWithModuleDefs extends ModuleTransformation {
  
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
      withSuper(super.transModule(name, is, mdefs)) {
        case Module(name, imports, defs) => {
          val allDefs = imports2defs(imports) ++ defs
          
          // we must also process imports inside strategy blocks
          val defsProcessedStrategies = allDefs map {
            case Strategy(stratName, stratImports, stratDefs) => 
              Strategy(stratName, Seq(), imports2defs(stratImports) ++ stratDefs)
            case other => other
          }
          
          Seq(Module(name, Seq(), defsProcessedStrategies))
        }
      }
  
  /**
   * recursively turns a list of imports into a list of ModuleDefs
   */
  private def imports2defs(imports: Seq[Import]): Seq[ModuleDef] = {
    imports flatMap {
      case Resolved(recursiveMod) => {
        val filteredModule = FilterGoalsAndLocals(Seq(recursiveMod))(config).head
        val recursiveDefs = imports2defs(filteredModule.imports)
        recursiveDefs ++ filteredModule.defs
      }
      case other => throw TransformationError("Expected all imports to be already resolved, got: " + other)
    }
  }
}