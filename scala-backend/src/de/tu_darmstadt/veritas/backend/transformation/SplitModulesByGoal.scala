package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef

object SplitModulesByGoal extends ModuleTransformation {
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case Goals(gs, t)                 => gs map (g => Goals(Seq(g), t))
      // TODO how do I split the Module into one Module per Goal with maximal code reuse? Is a new module created per one ModuleDef in the result sequence? 
    }
  }
}