package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{FixedVar, InductionHypothesis, StructInductCase, StructuralInductionHint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, Evidence, GenStepResult, StepResultProducer}

import scala.reflect.ClassTag


case class InductionSchemeEvidence[Defs, Formulae <: Defs](goal: Formulae,
                                                           indvar: Defs,
                                                           cases: List[StructInductCase[Defs, Formulae]]) extends Evidence with ResultDetails {

  private def printCase(si: StructInductCase[Defs, Formulae]): String =
    s"${si.casename}: \n" +
    s"\t Fixed Variables: ${(si.fixedvars map (_.fixedvar)).mkString(", ")} \n" +
    "\t Induction hypotheses: \n" +
    si.ihs.mkString("\t\n") +
    "\t Induction case: \n" + s"${goal}"


  override def toString: String = s"Goal to be proved via structural induction over variable $indvar: \n$goal\n" +
    "Induction cases: \n" +
    cases.map(printCase(_)).mkString("\n")

  /**
    *
    * @return full logs of prover (not applicable here)
    */
  override def fullLogs = this.toString

  override def summaryDetails = this.toString

  override def proofEvidence = Some(this)

  override def message = Some(s"Step is structural induction over variable $indvar")
}

/**
  * A simple verifier that is to be used on steps where an induction scheme is applied to an obligation
  * Will generate a result that contains the induction scheme as evidence and will always declare
  * such a step as verified, if such a result can be created.
  *
  * Users may inspect the induction scheme in the evidence in order to decide whether to trust this step or not.
  */
class TrustInductionSchemeVerifier[Defs : ClassTag, Formulae <: Defs : ClassTag] extends Verifier[Defs, Formulae] {

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

    //TODO maybe move these utility functions somewhere else later?!
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      }

    def matchEdgeOnStructInductCase(el: EdgeLabel): Option[StructInductCase[Defs, Formulae]] =
      el match {
        case si:StructInductCase[Defs, Formulae] => Some(si)
        case _ => None
      }

    val assmedgelist = (assumptions map (p => p._1)).toList

    // match all the assumptions' edge against StructInductCases (only succeed if matching successful for all edges!)
    val structinductcases: Option[List[StructInductCase[Defs,Formulae]]] =
      traverse(assmedgelist)(el => matchEdgeOnStructInductCase(el))

    (hints, structinductcases) match {
      case (Some(StructuralInductionHint(g: Formulae, indvar: Defs)), Some(caselist)) => {
        //construct induction evidence
        val evidence = InductionSchemeEvidence(g, indvar, caselist)
        produce.newStepResult(Finished(Proved(evidence), this), Some(evidence), None)
      }
      case _ => //let verifier produce an error message
        produce.newStepResult(VerifierFailure(s"No induction scheme found in this proof step, verifier not applicable.", this), None, None)
    }

  }
}
