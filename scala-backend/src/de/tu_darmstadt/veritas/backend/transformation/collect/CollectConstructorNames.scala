package de.tu_darmstadt.veritas.backend.transformation.collect

import de.tu_darmstadt.veritas.backend.veritas.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.veritas.ConstDecl
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.Local
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.veritas.Strategy
import de.tu_darmstadt.veritas.backend.veritas.Module

trait CollectConstructorNames extends ModuleTransformation {
  var consNames: Set[String] = Set()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    consNames = Set()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case l @ Local(defs) => {
        val oldCons = consNames
        val res = super.transModuleDefs(l)
        consNames = oldCons
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldCons = consNames
        val res = super.transModuleDefs(s)
        consNames = oldCons
        res
      }
      case m => super.transModuleDefs(m)
    }

  override def transDataTypeConstructor(d: DataTypeConstructor, open: Boolean, dataType: String): Seq[DataTypeConstructor] =
    withSuper(super.transDataTypeConstructor(d, open, dataType)) {
      case c@DataTypeConstructor(n, in) =>
        consNames = consNames + n
        Seq(c)
    }

  override def transConstDecl(d: ConstDecl): Seq[ConstDecl] = 
    withSuper(super.transConstDecl(d)) {
      case c@ConstDecl(n, out) =>
        consNames = consNames + n
        Seq(c)
    }
}
