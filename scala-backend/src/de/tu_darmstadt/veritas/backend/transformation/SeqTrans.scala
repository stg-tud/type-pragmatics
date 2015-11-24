package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.veritas.Module

case class SeqTrans(ts: Seq[ModuleTransformation]) extends ModuleTransformation {

  def this(ts: ModuleTransformation*) = this(Seq(ts:_*))

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] =
    ts.foldLeft(m)((m, t) => t.apply(m))
}

object SeqTrans {
  def apply(ts: ModuleTransformation*): SeqTrans = SeqTrans(Seq(ts:_*))
}