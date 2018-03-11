package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object ADTFailTypeParams extends SPLSpecification {
  @Open
  trait First[T] extends Expression
}
