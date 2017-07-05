package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast.{Module, VeritasConstruct}

import scala.util.{Failure, Success}

/**
  * Verifier for translating Veritas AST to TPTP (with selected transformation strategy) and calling
  * Vampire on the result
  */
class TPTPVampireVerifier(timeout: Int = 10, version: String = "4.1") extends TPTPVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPVampireVerifier"

  override def prover: Prover[TPTP] = Vampire(timeout = timeout, version = version)
}
