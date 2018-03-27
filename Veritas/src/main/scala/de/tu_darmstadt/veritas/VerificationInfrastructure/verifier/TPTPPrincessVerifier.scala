package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast.{Module, VeritasConstruct}

import scala.util.{Failure, Success}

/**
  * Created by andiderp on 06/04/2017.
  */
class TPTPPrincessVerifier(timeout: Int = 10) extends TPTPVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPPrincessVerifier"

  override def prover: Prover[TPTP] = Princess(timeout)
}
