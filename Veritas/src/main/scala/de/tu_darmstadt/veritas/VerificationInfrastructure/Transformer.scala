package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Translate a goal/problem P inside a specification S to a verification format V
  * that one or more provers understand
  */
trait Transformer[S, P, V] {
 def transformProblem(spec: S, goal: P) : V
}
