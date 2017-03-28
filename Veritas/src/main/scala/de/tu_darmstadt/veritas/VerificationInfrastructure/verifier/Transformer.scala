package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier


trait TransformerError

/**
  * Translate a goal/problem Goal inside a specification Spec to a verification format V
  * that one or more provers understand
  */
trait Transformer[Spec, Goal, V <: VerifierFormat] {
 def transformProblem(spec: Spec, goal: Goal) : Either[V, TransformerError]
}
