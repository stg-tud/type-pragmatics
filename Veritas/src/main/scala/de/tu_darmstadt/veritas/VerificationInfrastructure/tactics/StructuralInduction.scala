package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer

//TODO require Goal <: Spec everywhere in addition?
case class StructuralInduction[Defs <: Ordered[Defs], Formulae <: Defs](inductionvar: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

  import queryspec._

  //TODO isApplicable is a candidate for a common Tactic method
  /**
    * determines whether this tactic is applicable to a given goal
    *
    *
    * structural induction:
    * - goal has pattern "forall v1 iv v2. premises => conclusions (where v1, v2 are lists of variables,
    * and premises/conclusions are conjuncts of expressions; premises may be the empty list, conclusions must not be empty)
    * - induction variable is of type "closed ADT"
    * - premises or conclusions of goal contain calls to recursive functions/jugdments
    */
  def isApplicable(g: Formulae): Boolean = {
    val functioncall_with_inductionvar: Option[Defs] = extractFunctionCalls(g) find (fc => getArguments(fc) contains inductionvar)
    goalMatchesPattern(g) &&
      (getUniversallyQuantifiedVars(g) contains inductionvar) &&
      isClosedADT(inductionvar) &&
      functioncall_with_inductionvar.isDefined
    //TODO: refine conditions, if necessary
  }

  //TODO goalMatchesPattern is a candidate for a common DSStrategy method
  // idea: introduce a small pattern language to describe goal patterns?
  def goalMatchesPattern(goal: Formulae): Boolean = ???
//  def goalMatchesPattern(g: Goal): Boolean = isForall(g) && {
//    val body = forallBody(g)
//    isImplication(body)
//
//  }
//
//  isEquivalent(g,
//    makeForall(extractUniversallyQuantifiedVars(g), makeImplication(extractPremises(g), extractConclusions(g))))



  //  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] = {
  //     val goal = obl.goal
  //    if (isApplicable(goal)) {
  //      val iv_cases = getCases(inductionvar)
  //      val induction_subgoals : Seq[Goal] = iv_cases map (ic => specToGoal(makeForall(extractUniversallyQuantifiedVars(goal),
  //        makeImplication(makeEquation(inductionvar, ic) +: extractPremises(goal), extractConclusions(goal)))))
  //      //TODO: create the induction hypotheses
  //      val fixed_vars: Seq[Option[FixedVars[Spec]]] = iv_cases map (ic => {
  //        val rarg = getRecArgs(ic)
  //        if (rarg.isEmpty) None else Some(FixedVars(rarg))
  //      })
  //      val final_ihs : Seq[StructInductCase[Spec]] = ???
  //      val finalinductioncases: Seq[(Goal, StructInductCase[Spec])] = induction_subgoals zip final_ihs
  //      pg.applyTactic(obl, OLDStructuralInduction[Spec, Goal](spec, finalinductioncases /*all the induction cases*/))
  //      pg
  //    }
  //    else
  //      pg //TODO throw an exception that explains why the strategy failed


  //  }
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
  override def apply[Obligation](obl: GenObligation[Defs, Formulae], obllabels: Iterable[EdgeLabel], produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = ???

  override def compare(that: Tactic[Defs, Formulae]): Int = ???
}
