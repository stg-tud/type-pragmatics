package de.tu_darmstadt.veritas.backend.transformation.imports

import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.Resolved
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.veritas.Unresolved
import de.tu_darmstadt.veritas.backend.veritas.ModuleDefHolder

/**
 * Recursively resolves the imports (but does not follow cyclic ones) in the given module.
 * That is, after this transformation, the Module and Strategy blocks contains only Resolved imports
 * (no longer Unresolved(<taskid>) or Unresolved(<module URL>) ones), where the code of the imported 
 * Module is attached.
 * 
 * To _remove_ the imports and _insert_ the relevant ModuleDefs into the given Module body, use
 * ReplaceImportsWithModuleDefs transformation.
 */
object ResolveImports extends ModuleTransformation {
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
      withSuper(super.transModule(name, is, mdefs)) {
        case mod: Module => {
          // add this module to the "already resolved set"
          val (resolvedImports, processedDefs) = resolveRecursive(mod, Set(mod.name))
          Seq(Module(mod.name, resolvedImports, processedDefs))
        }
      }

  /**
   * Resolves the imports of the given Module/Strategy. Then goes into the resolved Modules and
   * resolves their imports recursively. Also goes into all Strategy blocks and resolves their imports.
   * 
   * @param alreadyResolved keeps track of all Modules that are already resolved (i.e. for cycle detection)
   * @return a pair of the now resolved (Imports, Defs). Defs because they contain Strategy blocks that have
   * imports of their own.
   */
  private def resolveRecursive(mod: ModuleDefHolder, alreadyResolved : Set[String]): (Seq[Import], Seq[ModuleDef]) = {
    // resolve all top-level imports
    val resolvedTopLevelImports = mod.imports collect {
      case imp: Unresolved => imp.resolve()
      case imp: Resolved => imp
    }
    
    // sort out all we wont recursively resolve (== have already)
    val (wontResolve, toRecursiveResolve) = resolvedTopLevelImports.partition(alreadyResolved contains _.moduleName)

    // recurse into the resolved imports
    val recursivelyResolved = toRecursiveResolve map ( imp => {
      val (recursiveResolvedImports, recursiveResolvedDefs) = resolveRecursive(imp.moduleCode, alreadyResolved + imp.moduleName)
      Resolved(Module(imp.moduleCode.name, recursiveResolvedImports, recursiveResolvedDefs))
    })
    
    // resolve imports in the strategy blocks
    val defsWithResolvedImports = mod.defs map {
      case strat: Strategy => {
        val (recursiveResolvedImports, recursiveResolvedDefs) = resolveRecursive(strat, alreadyResolved)
        Strategy(strat.name, recursiveResolvedImports, recursiveResolvedDefs)
      }
      case other => other
    }

    (recursivelyResolved ++ wontResolve, defsWithResolvedImports)
  }
}