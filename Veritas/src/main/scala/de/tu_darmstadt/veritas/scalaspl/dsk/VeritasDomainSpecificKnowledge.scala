package de.tu_darmstadt.veritas.scalaspl.dsk

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.DomainSpecificKnowledge
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionSig}

trait VeritasDomainSpecificKnowledge extends DomainSpecificKnowledge[DataType, FunctionDef, TypingRule] {
  override def retrieveFunName(fd: FunctionDef): String = fd.signature.name

  override def retrievePropName(p: TypingRule): String = p.name
}
