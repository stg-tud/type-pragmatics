package de.tu_darmstadt.veritas.backend.transformation.collect

import de.tu_darmstadt.veritas.backend.ast.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.ast.ConstDecl
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.ast.Local
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.ast.Strategy
import de.tu_darmstadt.veritas.backend.ast.Module

trait CollectConstructorNames extends ModuleTransformation {
  var constructorNames: Set[String] = Set()
  var constNames: Set[String] = Set()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    constructorNames = Set()
    constNames = Set()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case l @ Local(defs) => {
        val oldConstructors = constructorNames
        val oldConsts = constNames
        val res = super.transModuleDefs(l)
        constructorNames = oldConstructors
        constNames = oldConsts
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldConstructors = constructorNames
        val oldConsts = constNames
        val res = super.transModuleDefs(s)
        constructorNames = oldConstructors
        constNames = oldConsts
        res
      }
      case m => super.transModuleDefs(m)
    }

  override def transDataTypeConstructor(d: DataTypeConstructor, dataType: String): Seq[DataTypeConstructor] =
    withSuper(super.transDataTypeConstructor(d, dataType)) {
      case c@DataTypeConstructor(n, in) =>
        constructorNames = constructorNames + n
        Seq(c)
    }

  override def transConstDecl(d: ConstDecl): Seq[ConstDecl] = 
    withSuper(super.transConstDecl(d)) {
      case c@ConstDecl(n, out) =>
        constNames = constNames + n
        Seq(c)
    }
}
