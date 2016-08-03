package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.ast.Module

case class SeqTrans(ts: ModuleTransformation*) extends ModuleTransformation {

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] =
    ts.foldLeft(m)((m, t) => t.apply(m))
}
