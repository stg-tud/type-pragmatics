package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.ast.Module

case class Optional(t: ModuleTransformation, cond: Configuration => Boolean) extends ModuleTransformation {
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] =
    if (cond(config))
      t.apply(m)
    else
      m
}