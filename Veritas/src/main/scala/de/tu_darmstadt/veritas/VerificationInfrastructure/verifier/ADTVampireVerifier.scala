package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * Verifier for translating Veritas AST to SMTLib (with selected transformation strategy) and calling
  * Vampire on the result
  */
class ADTVampireVerifier(timeout: Int = 10) extends SMTLibVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "ADTVampireVerifier"

  override def prover: Prover[SMTLibFormat] = Vampire4_1_tar(timeout = timeout)
}
