package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.StepResultProducer

trait VerifierFormat

/**
  * Verifiers "manage" verification attempts (i.e. compiling the problem, calling one or more provers,
  * starting/stopping a proof attempt...)
  *
  */
trait Verifier[Spec, Goal] extends Ordered[Verifier[Spec, Goal]] {
  type V <: VerifierFormat

  /** Textual description that should be unique (used for ordering verifiers) */
  val desc: String

  override def compare(that: Verifier[Spec, Goal]): Int = this.desc compare that.desc

  /**
    * A concrete verifier may call any combination of transformers & provers
    * (or do something else to produce a verification result)
    * @param goal
    * @param spec
    * @param assumptions
    * @param produce
    * @tparam Result
    * @return
    */
  def verify[Result](goal: Goal, spec: Spec, assumptions: Iterable[Goal],
                     produce: StepResultProducer[Spec, Goal, Result]): Result

}
