package de.tu_darmstadt.veritas.backend.transformation.imports

import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.ModuleDefHolder
import de.tu_darmstadt.veritas.backend.veritas.Resolved
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.veritas.Unresolved

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
    
    // remove duplicate imports
    val resolvedTopLevelImportsUnique = resolvedTopLevelImports.distinct

    // sort out all we wont resolve (== have already)
    val (wontResolve, toRecursiveResolve) = resolvedTopLevelImportsUnique.partition(alreadyResolved contains _.moduleName)
    
    // recurse into the resolved imports
    val recursivelyResolved = toRecursiveResolve map ( imp => {
      // add all top-level import to the "already done" list (for recursive resolves), except our own import
      val recursiveAlreadyResolved = alreadyResolved ++ toRecursiveResolve.map(_.moduleName).filterNot(_ == imp.moduleName)
      val (recursiveResolvedImports, recursiveResolvedDefs) = resolveRecursive(imp.moduleCode, recursiveAlreadyResolved)
      Resolved(Module(imp.moduleCode.name, recursiveResolvedImports, recursiveResolvedDefs))
    })
    
    // resolve imports in the strategy blocks
    val defsWithResolvedImports = mod.defs map {
      case strat: Strategy => {
        // do not re-resolve imports in strategies already resolved in the parent module
        val recursiveAlreadyResolved = alreadyResolved ++ recursivelyResolved.map(_.moduleName)
        val (recursiveResolvedImports, recursiveResolvedDefs) = resolveRecursive(strat, recursiveAlreadyResolved)
        Strategy(strat.name, recursiveResolvedImports, recursiveResolvedDefs)
      }
      case other => other
    }

    (recursivelyResolved, defsWithResolvedImports)
  }
}