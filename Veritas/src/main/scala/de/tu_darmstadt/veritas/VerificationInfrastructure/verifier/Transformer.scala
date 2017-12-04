package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.EdgeLabel
import scala.util.Try


trait TransformerError extends Exception

/**
  * Translate a goal/problem Goal inside a specification Spec to a verification format V
  * that one or more provers understand
  */
trait Transformer[Spec, Goal, V <: VerifierFormat] {

  /**
    * from an obligation node and the corresponding edges to the node and its children (assumptions),
    * assemble the full proof problem
    *
    * this step incorporates proof information that is propagated along the proof graph (e.g.
    * fixed variables, induction hypotheses) into the final proof problem, which then becomes
    * Spec => (single) Goal and potentially uses scoping constructs from the input format, to deal with
    * fixed variables etc.
    *
    * @param goal
    * @param spec
    * @param parentedges
    * @param assumptions
    * @return
    */
  def assembleFullProblem(goal: Goal, spec: Spec,
                          parentedges: Iterable[EdgeLabel],
                          assumptions: Iterable[(EdgeLabel, Goal)]): (Spec, Spec, Goal)


  /**
    * given a fully assembled proof problem of the form Spec => (single) Goal, translate to
    * the final verification format
    *
    * @param problem
    * @return
    */
  def translateProblem(problem: (Spec, Spec, Goal)): Try[V]


  def transformProblem(goal: Goal, spec: Spec,
                       parentedges: Iterable[EdgeLabel],
                       assumptions: Iterable[(EdgeLabel, Goal)]): Try[V] =
    translateProblem(assembleFullProblem(goal, spec, parentedges, assumptions))
}
