package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * Created by andiderp on 06/04/2017.
  */
class TPTPEproverVerifier(timeout: Int = 10) extends TPTPVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPEproverVerifier"

  override def prover: Prover[TPTP] = Eprover(timeout)
}