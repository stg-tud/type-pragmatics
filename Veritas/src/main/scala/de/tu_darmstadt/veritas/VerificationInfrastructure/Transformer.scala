package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Translate a goal/problem P inside a specification S to a verification format V
  * that one or more provers understand
  */
abstract class Transformer[S, P, +V](spec: S, goal: P) {
 def transformProblem : V
}
