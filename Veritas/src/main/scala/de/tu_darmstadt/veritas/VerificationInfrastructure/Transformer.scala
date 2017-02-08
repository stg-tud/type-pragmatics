package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * transform a problem specification (S) and a goal (P) into a verification format (V) that
  * one or more provers can process
  */
abstract class Transformer[S, P, +V](spec: S, goal: P) {
  def transformProblem: V
}
