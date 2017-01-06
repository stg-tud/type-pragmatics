package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Created by sylvia on 14/10/16.
  */
abstract class Prover[V](problem: V) {
  def getResult(): Boolean
  def makeCall()

}
