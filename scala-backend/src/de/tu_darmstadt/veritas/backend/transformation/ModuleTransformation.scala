package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef

trait ModuleTransformation {
  /* abstract */ def apply(input: Module): Module
}

trait ModuleDefTransformation extends ModuleTransformation {
  final override def apply(input: Module): Module = {
    Module(input.name,
           input.imports,
           input.body.map(apply))
  }
  
  protected /* abstract */ def apply(input: ModuleDef): ModuleDef
}