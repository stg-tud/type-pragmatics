package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.Constructors
import de.tu_darmstadt.veritas.backend.veritas.ConstructorDecl
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef

/**
 * Collects all Constructors(Seq(...)) ModuleDefs into a single Constructors(<concat of all Seqs>)
 * ModuleDef in this Module. The joined Constructors() will be at the place of the first Constructor()
 * ModuleDef, all remaining Constructors() are removed from the Module.body
 */
object JoinConstructors extends ModuleTransformation {
  override def apply(input: Module): Module = {
    val (nonCtorsPrefix, rest) = input.body span (!_.isInstanceOf[Constructors])
    
    val collectedCtorDecls = collection.mutable.ListBuffer.empty[ConstructorDecl]
    val nonCtorsSuffix = collection.mutable.ListBuffer.empty[ModuleDef]
    rest foreach {
      case Constructors(decls) => collectedCtorDecls ++= decls
      case other => nonCtorsSuffix += other
    }

    Module(input.name, input.imports, 
        nonCtorsPrefix ++ (Constructors(collectedCtorDecls) +: nonCtorsSuffix))
  }
}

// FIXME Problem: cannot cast/isInstanceOf on abstract type T due to type erasure -.-
// how to implement this?
trait JoinModuleDefTransformation[T <: ModuleDef] extends ModuleTransformation {
  override def apply(input: Module): Module
}