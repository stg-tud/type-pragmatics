package de.tu_darmstadt.veritas.backend.transformation.imports

import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
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
        case mod: Module => Seq(resolveRecursiveModule(mod, Set.empty))
      }
  
  private def resolveRecursiveModule(mod: Module, alreadyResolved_ : Set[String]): Module = {
    // resolve all top-level imports
    val resolvedTopLevelImports = mod.imports collect {
      case imp: Unresolved => imp.resolve()
      case imp: Resolved => imp
    }
    
    // add this module to the "already resolved set"
    val alreadyResolved = alreadyResolved_ + mod.name
    
    // sort out all we wont recursively resolve (== have already)
    val (wontResolve, toRecursiveResolve) = resolvedTopLevelImports.partition(alreadyResolved contains _.moduleName)

    // recurse into the resolved imports
    val recursivelyResolved = toRecursiveResolve.map(
      imp => Resolved(resolveRecursiveModule(imp.moduleCode, alreadyResolved)))
    
    // resolve imports in the strategy blocks
    val defsWithResolvedImports = mod.defs map {
      case s: Strategy => resolveRecursiveStrategy(s, alreadyResolved)
      case other => other
    }

    Module(mod.name, recursivelyResolved ++ wontResolve, defsWithResolvedImports)
  }
  
  private def resolveRecursiveStrategy(strat: Strategy, alreadyResolved: Set[String]): Strategy = {
    // resolve all top-level imports
    val resolvedTopLevelImports = strat.imports collect {
      case imp: Unresolved => imp.resolve()
      case imp: Resolved => imp
    }
    
    // sort out all we wont recursively resolve (== have already)
    val (wontResolve, toRecursiveResolve) = resolvedTopLevelImports.partition(alreadyResolved contains _.moduleName)

    // recurse into the resolved imports
    val recursivelyResolved = toRecursiveResolve.map(
      imp => Resolved(resolveRecursiveModule(imp.moduleCode, alreadyResolved)))
    
    // resolve imports in the strategy blocks
    val defsWithResolvedImports = strat.defs map {
      case s: Strategy => resolveRecursiveStrategy(s, alreadyResolved)
      case other => other
    }

    Strategy(strat.name, recursivelyResolved ++ wontResolve, defsWithResolvedImports)
  }
}