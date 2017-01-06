package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Created by sylvia on 14/10/16.
  */
abstract class Verifier[S, P] {
  type V //Representation of Verification format
  val transformer : Transformer[S, P, V]
  val provers : Seq[Prover[V]]
  val supportedStrategies: Seq[VerificationStrategy]

  def verify(spec: S, hypotheses: Seq[P], goal: P, strat: VerificationStrategy): Boolean

}
