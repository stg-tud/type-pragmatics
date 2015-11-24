package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.veritas.Module

case class Alternative(select: Configuration => ModuleTransformation) extends ModuleTransformation {
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] =
    select(config).apply(m)
}