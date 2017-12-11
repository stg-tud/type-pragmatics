package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * Verifier for translating Veritas AST to SMTLib (with selected transformation strategy) and calling
  * Vampire on the result
  */
class Z3VampireVerifier(timeout: Int = 10) extends SMTLibVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "Z3VampireVerifier"

  override def prover: Prover[SMTLibFormat] = VampireZ3(timeout = timeout)
}
