package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * Expects the correct Vampire binaries in the project path, named "vampire-3.0", "vampire-4.0" or "vampire-4.1"
  *
  * @param timeout in seconds
  */
case class Vampire(version: String, timeout: Int) extends Prover[TPTP] {
  override def callProver(problem: TPTP): ProverStatus = ???
}
