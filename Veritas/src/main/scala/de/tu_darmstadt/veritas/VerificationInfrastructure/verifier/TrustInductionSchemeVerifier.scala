package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{FixedVar, InductionHypothesis, StructInductCase}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}

/**
  * A simple verifier that is to be used on steps where an induction scheme is applied to an obligation
  * Will generate a result that contains the induction scheme as evidence and will always declare
  * such a step as verified, if such a result can be created.
  *
  * Users may inspect the induction scheme in the evidence in order to decide whether to trust this step or not.
  */
class TrustInductionSchemeVerifier[Defs, Formulae <: Defs] extends Verifier[Defs, Formulae] {

  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc = "TrustInductionSchemeVerifier"

  /**
    * A concrete verifier may call any combination of transformers & provers
    * (or do something else to produce a verification result)
    *
    * Edge Labels from proof graphs can contain hints for verification (e.g. fixed variables, induction scheme
    * verify may take hints from the caller (i.e. the tactic;
    * a hint could for example be an induction scheme, prover timeout etc.
    * etc.). A concrete verifier can define how to use these hints when verifying.
    *
    *
    * //TODO maybe make VerifierHints less general?
    * //TODO maybe introduce a mechanism for letting a Verifier "announce" to a caller what hints it requires?
    *
    * @param goal
    * @param spec
    * @param parentedges
    * @param assumptions
    * @param hints
    * @param produce
    * @tparam Result
    * @return
    */
  override def verify[Result <: GenStepResult[Defs, Formulae]](goal: Formulae,
                                                               spec: Defs,
                                                               parentedges: Iterable[EdgeLabel],
                                                               assumptions: Iterable[(EdgeLabel, Formulae)],
                                                               hints: Option[VerifierHints],
                                                               produce: StepResultProducer[Defs, Formulae, Result],
                                                               pathforlogs: Option[String]): Result = {
    //    def matchEdge(el: EdgeLabel): Option[(String, (Seq[FixedVar[Defs]], Seq[InductionHypothesis[Formulae]]))] =
    //      el match {
    //        case StructInductCase(name, fvs : Seq[FixedVar[Defs]], ihs : Seq[InductionHypothesis[Formulae]], pinf) => Some(name -> (fvs, ihs))
    //        case _ => None
    //      }

    val structinductcases: Option[Map[String, (Seq[FixedVar[Defs]], Seq[InductionHypothesis[Defs]])]] =
      ???

    ???

  }
}
