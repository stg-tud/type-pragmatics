package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Created by sylvia on 14/10/16.
  */
abstract class Transformer[S, P, V](spec: S, goal: P) {
 def transformProblem : V
}
