package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.Local
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.GoalsWithStrategy

trait FilterModuleDefs extends ModuleTransformation {
  /**
   * @return true if you want to keep this element, false if it should get removed
   */
  /* abstract */ def filterPredicate(x: ModuleDef): Boolean
  
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = 
    withSuper(super.transModuleDefs(mdef)) { 
      case mdef => 
        if (filterPredicate(mdef))
          Seq(mdef)
        else
          Seq()
    }
}

object FilterGoalsAndLocals extends FilterModuleDefs {
  override def filterPredicate(x: ModuleDef) = x match {
    case _: Local | _: Goals | _:GoalsWithStrategy => false
    case _ => true
  }
}
