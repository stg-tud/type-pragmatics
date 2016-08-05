package de.tu_darmstadt.veritas.backend.transformation.collect

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.ast.Local
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.SortDef
import de.tu_darmstadt.veritas.backend.ast.Strategy


trait CollectSortNames extends ModuleTransformation {
  var sortNames: Set[String] = Set()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    sortNames = Set()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case l @ Local(defs) => {
        val oldCons = sortNames
        val res = super.transModuleDefs(l)
        sortNames = oldCons
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldCons = sortNames
        val res = super.transModuleDefs(s)
        sortNames = oldCons
        res
      }
      case m => super.transModuleDefs(m)
    }

  override def transSortDefs(sd: SortDef): Seq[SortDef] =
    withSuper(super.transSortDefs(sd)) {
      case s @ SortDef(_) => {
        sortNames = sortNames + sd.name
        Seq(s)
      }
    }

}

