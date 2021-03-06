package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Verifier}

/**
  * Tactics for labeling edges of ProofTrees
  */
trait Tactic[Spec, Goal] extends Serializable {

  //obtain propagatable info from incoming edges and forward if there are no conflicts
  protected def obtainPropagatableInfo[Obligation](obllabels: Iterable[EdgeLabel]) = {

    val full_propagatedInfo = obllabels map (el => el.propagateInfoList)

    val all_same_elements = (full_propagatedInfo map (ps => ps.toSet)).toSeq.distinct

    val propagatedInfo =
      if (full_propagatedInfo.isEmpty || all_same_elements.size > 1)
        Seq()
      else full_propagatedInfo.head

    propagatedInfo
  }


  /**
    * checks whether a given obligation is verified with regard to the tactic
    *
    * the default implementation simply requires all given edges to be verified
    * other concrete implementations may decide to behave differently be overriding this method
    *
    * @param g
    * @param obl
    * @param edges
    * @return
    */
  def allRequiredOblsVerified(g: IProofGraph[Spec, Goal])
                             (obl: g.Obligation,
                              edges: Iterable[(g.Obligation, EdgeLabel)]): Boolean =
    edges.forall { case (subobl, label) => g.isOblVerified(subobl) }

  /**
    * verifying a step via its edges generates a step result
    * the caller has to decide whether this result will be integrated into a proof graph or not
    *
    * for verifying a step, a tactic may generate a "hint" for the given verifier (depending on the goal and on
    * the given verifier
    *
    * the default implementation simply calls the given verifier with all given edges and no hints;
    * other concrete implementations may decide to behave differently by overriding this method
    *
    * @param obl
    * @param parentedges
    * @param subobl
    * @param verifier
    * @return
    */
  def verifyStep[Result <: GenStepResult[Spec, Goal]](obl: GenObligation[Spec, Goal],
                                                      parentedges: Iterable[EdgeLabel],
                                                      subobl: Iterable[(EdgeLabel, GenObligation[Spec, Goal])],
                                                      verifier: Verifier[Spec, Goal],
                                                      produce: StepResultProducer[Spec, Goal, Result],
                                                      pathforlogs: Option[String] = None): Result =
    verifier.verify(obl.goal, obl.spec, parentedges, subobl.map { case (e, so) => (e, so.goal) }, None, produce, pathforlogs)

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  def apply[Obligation](obl: GenObligation[Spec, Goal],
                        obllabels: Iterable[EdgeLabel],
                        produce: ObligationProducer[Spec, Goal, Obligation]): Iterable[(Obligation, EdgeLabel)]
}

trait TacticApplicationException[Spec, Goal] extends Exception {
  val tactic: Tactic[Spec, Goal]
}