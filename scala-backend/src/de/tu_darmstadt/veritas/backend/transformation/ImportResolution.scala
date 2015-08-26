package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.Resolved
import de.tu_darmstadt.veritas.backend.veritas.Unresolved

object ImportResolution {
  /**
   * (Recursively) resolves the imports (but does not follow cyclic ones) in the given module
   */
  def resolveImports(toResolve: Module) = resolveRecursive(toResolve, Set.empty)  
  private def resolveRecursive(mod: Module, already: Set[String]): Module = {
    // resolve all top-level imports
    val resolvedTopLevelImports = mod.imports collect {
      case imp: Unresolved => imp.resolve()
      case imp: Resolved => imp
    }
    
    // sort out all we wont recursively resolve (== have already)
    val (wontResolve, toRecursiveResolve) = resolvedTopLevelImports.partition(already contains _.moduleName)

    // recurse into the resolved imports
    val recursivelyResolved = toRecursiveResolve.map(
      imp => Resolved(resolveRecursive(imp.moduleCode, already + mod.name)))
    
    Module(mod.name, recursivelyResolved ++ wontResolve, mod.body)
  }
  
  /**
   * Collects all (recursive) resolved imports by going into the imported modules 
   * (ignores the unresolved imports)
   */
  def collectResolvedImports(mod: Module): Seq[Resolved] = collectResolvedImports(mod.imports)
  def collectResolvedImports(imps: Seq[Import]): Seq[Resolved] = imps.foldLeft[Seq[Resolved]](Nil) {
    // NOTE ImportResolution.resolveImports() should never resolve a module twice anywhere in the
    // tree, so we refrain from checking here that the import was not already resolved elsewhere.
    case (resolvedSoFar, imp: Resolved) => 
      (imp +: collectResolvedImports(imp.moduleCode)) ++ resolvedSoFar
    case (resolvedSoFar, _) => resolvedSoFar
  }
  
  /**
   * Pulls all recursive imports to the top level import list
   */
  def flattenResolvedImports(mod: Module) = Module(mod.name, collectResolvedImports(mod.imports), mod.body)
}