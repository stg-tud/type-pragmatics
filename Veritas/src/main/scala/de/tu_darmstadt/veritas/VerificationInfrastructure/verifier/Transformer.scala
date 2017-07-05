package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.EdgeLabel
import scala.util.Try


trait TransformerError extends Exception

/**
  * Translate a goal/problem Goal inside a specification Spec to a verification format V
  * that one or more provers understand
  */
trait Transformer[Spec, Goal, V <: VerifierFormat] {
  def transformProblem(goal: Goal, spec: Spec,
                       parentedges: Iterable[EdgeLabel],
                       assumptions: Iterable[Goal]): Try[V]
}
