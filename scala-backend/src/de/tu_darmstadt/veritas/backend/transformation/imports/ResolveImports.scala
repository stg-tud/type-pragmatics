package de.tu_darmstadt.veritas.backend.transformation.imports

import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.ModuleDefHolder
import de.tu_darmstadt.veritas.backend.veritas.Resolved
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.veritas.Unresolved
import de.tu_darmstadt.veritas.backend.veritas.Unresolved
import de.tu_darmstadt.veritas.backend.veritas.Unresolved
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.transformation.FilterGoalsAndLocals

/**
 * Recursively resolves imports and inlines the bodies of imported modules. Also handles imports within Strategy blocks.
 * This transformation does *not* detect cyclic imports and will loop if one occurs.
 */
object ResolveImports extends ModuleTransformation {
  
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
      withSuper(super.transModule(name, is, mdefs)) {
        case mod: Module => Seq(resolve(mod))
      }

  private def resolve(mod: Module): Module = {
    //val importedDefs = resolve(Seq(Resolved(mod)))
    val importedDefs = resolve(mod.imports) 
    
    val resolvedBodyDefs = mod.defs map {
      case Strategy(name, imps, defs) =>
        val importedDefs = resolve(imps)
        Strategy(name, Seq(), importedDefs ++ defs)
      case other => other
    }

    Module(mod.name, Seq(), importedDefs ++ resolvedBodyDefs)
  }
    
  private def resolve(_imps: Seq[Import]): Seq[ModuleDef] = {
    var todo = _imps
    var done = Set[String]()
    var result = Seq[ModuleDef]()

    def push(imp: Import) = 
      todo = imp +: todo

    def pop() = {
      var imp = todo.head
      todo = todo.tail
      imp
    }
    
    while (!todo.isEmpty) {
      val imp = pop()
		  val m = imp match {
  		  case imp: Unresolved => (FilterGoalsAndLocals.trans(imp.resolve.moduleCode)).head
  		  case imp: Resolved => (FilterGoalsAndLocals.trans(imp.moduleCode)).head
      }
      if (!done.contains(m.name)) {
        if (m.imports.isEmpty) {
          done += m.name
          result ++= m.defs
        }
        else {
          // push m to remember that m's mdefs need to be added after resolving all required modules 
      	  push(Resolved(Module(m.name, Seq(), m.defs)))
          m.imports foreach (push(_))
        }
      }
    }
    
    // resolve imports within strategies
    result map {
      case Strategy(name, imps, defs) =>
        val importedDefs = resolve(imps)
        Strategy(name, Seq(), importedDefs ++ defs)
      case other => other
    }
  }
  

}