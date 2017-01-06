package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Import, Module, ModuleDef, Resolved}

/**
  * DSL for creating modules
  * TODO: Module creation only succeeds of module can be type-checked
  */
object ModuleDSL {

  def imports(mds: Module*) = Seq(mds map (Resolved(_)))

  def module(name: String, imps: Seq[Import] = Seq())(mds: ModuleDef*) =
    Module(name, imps, mds)

}
