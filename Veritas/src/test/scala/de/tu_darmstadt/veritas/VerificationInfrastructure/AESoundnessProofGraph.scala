package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import scala.meta.Source
import scala.meta.parsers.Parsed


class AESoundnessProofGraph(file: File) {
  val g: ProofGraphXodus[Parsed[Source], Parsed[Source]] = ???

}
