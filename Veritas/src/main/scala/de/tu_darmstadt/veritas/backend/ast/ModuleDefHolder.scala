package de.tu_darmstadt.veritas.backend.ast

trait ModuleDefHolder {
  /**
   * should be empty Seq() for local
   */
  /* abstract */ def imports: Seq[Import]
  /* abstract */ def defs: Seq[ModuleDef]
}