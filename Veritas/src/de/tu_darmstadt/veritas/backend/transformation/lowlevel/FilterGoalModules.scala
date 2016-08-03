package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.ast.Import
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.Goals
import de.tu_darmstadt.veritas.backend.ast.Lemmas
import de.tu_darmstadt.veritas.backend.Configuration

trait ContainsGoal extends ModuleTransformation {
  private var _containsGoal: Boolean = false

  def containsGoal: Boolean = _containsGoal

  override def transModule(name: String, _is: Seq[Import], _mdefs: Seq[ModuleDef]): Seq[Module] = {
    _containsGoal = false
    super.transModule(name, _is, _mdefs)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case g @ Goals(_, _) => { _containsGoal = true; Seq(g) }
      case l @ Lemmas(_, _) => { _containsGoal = true; Seq(l) }
    }

}

object FilterGoalModules extends ModuleTransformation with ContainsGoal {

  override def transModule(name: String, _is: Seq[Import], _mdefs: Seq[ModuleDef]): Seq[Module] = {
    val restemp = super.transModule(name, _is, _mdefs)
    if (containsGoal)
      restemp
    else Seq()
  }

}