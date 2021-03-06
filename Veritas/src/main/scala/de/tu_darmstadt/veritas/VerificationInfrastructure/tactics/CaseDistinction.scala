package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure._


// General case distinction tactic
// will not modify the cases map at all - expects case premises as they will have to be in the proof in the end
// will prepend goal name and "-case-" to case names but not generate any appropriate case names
// global_additional_premises: Premises that are going to be added to each case. Especially premises for let-bindings of variables
case class CaseDistinction[Defs, Formulae <: Defs](cases: Map[String, Seq[Formulae]],
                                                   queryspec: SpecEnquirer[Defs, Formulae],
                                                   rec_calls: Seq[String] = Seq(),
                                                   fc_calls: Seq[String] = Seq(),
                                                   new_global_additional_premises: Seq[Formulae] = Seq()) extends Tactic[Defs, Formulae] {

  import queryspec._

  // assume that goal is a quantified expression
  def isApplicable(g: Formulae): Boolean = isQuantified(g)


  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    val goal = obl.goal
    val goalbody = getQuantifiedBody(goal)
    val prems = getPremises(goalbody) //could be empty, which is ok (then goalbody becomes an implication)
    val concs = getConclusions(goalbody)

    // assumes that the variables within the cases are already appropriately named
    // with regard to the remaining variables in the goal (no unwanted name clashes!)

    val case_subgoals: Seq[Formulae] =
      (cases map { case (n, c) => {
        //simply add each goal to the premises
        val added_premises = c ++ prems ++ new_global_additional_premises
        //reassemble goal and attach name
        val casename = "-" + n
        makeNamedGoal(makeForallQuantifyFreeVariables(
          makeImplication(added_premises, concs)), getFormulaName(goal) ++ casename)
      }
      }).toSeq

    val propagatedInfo: Seq[PropagatableInfo] = obtainPropagatableInfo(obllabels)
    val recfc_edgelabels = for (fn <- rec_calls) yield RecursiveCall(fn)
    val fc_edgelabels = for (fn <- fc_calls) yield FunctionCall(fn)

    val edgelabels = for (cs <- case_subgoals) yield
      CaseDistinctionCase(getFormulaName(cs), recfc_edgelabels, fc_edgelabels, propagatedInfo)

    for ((caseobl, edge) <- case_subgoals zip edgelabels) yield {
      val newobl = produce.newObligation(queryspec.fullspec, caseobl, edge.casename)
      (newobl, edge)
    }
  }

  //call with a parent obligation and the sub-obligations that this parent requires
  //will try to match the cases against induction cases and then assemble a map from case names to (Obligation, EdgeLabel)
  def enumerateCases[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Map[String, (Obligation, EdgeLabel)] = {
    (for (r <- required) yield {
      r._2 match {
        case CaseDistinctionCase(name, _, _, _) => name -> r
        case c => sys.error(s"Enumerate cases of a case distinction: The given required sub-obligations were not all labeled as case distinction cases: $c")
      }
    }).toMap
  }

  def enumerateCaseNames[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Seq[String] =
    enumerateCases(required).keys.toSeq.sortWith(_ < _) //alphabetical ordering

  def selectCase[Obligation](casename: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
    enumerateCases(required)(casename)._1
}

case class StructuralCaseDistinction[Defs, Formulae <: Defs](distvar: Defs,
                                                             queryspec: SpecEnquirer[Defs, Formulae],
                                                             new_global_additional_premises: Seq[Formulae] = Seq()) extends Tactic[Defs, Formulae] {

  import queryspec._

  def isApplicable(g: Formulae): Boolean = isClosedADT(distvar, g) && isQuantified(g)

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
  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    val goal = obl.goal
    val goalbody = getQuantifiedBody(goal)
    if (isApplicable(goal)) {
      //make sure that variable names of cases do not clash with variables names in goal
      val dist_cases_defs_renamed = getCases(distvar, goalbody) map { case (n, c) => (n, assignCaseVariables(c, goalbody)) }
      val dist_cases = dist_cases_defs_renamed map {case (n, dc) => (n, Seq(makeEquation(distvar, dc))) }
      //for structural case distinctions, we typically do not need to propagate any function calls
      CaseDistinction[Defs, Formulae](dist_cases, queryspec, Seq(), Seq(), new_global_additional_premises)(obl, obllabels, produce)
    } else
      Seq() //TODO throw an exception that explains why the tactic failed
  }
}

case class EqualityCaseDistinction[Defs, Formulae <: Defs](lhs: Defs, rhs: Defs,
                                                           queryspec: SpecEnquirer[Defs, Formulae],
                                                           rec_calls: Seq[String] = Seq(),
                                                           fc_calls: Seq[String] = Seq(),
                                                           new_global_additional_premises: Seq[Formulae] = Seq()) extends Tactic[Defs, Formulae] {
  import queryspec._

  // TODO: check
  def isApplicable(g: Formulae): Boolean = true

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
  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {

    //TODO better names for equations?
    val dist_cases = Map(("Eq-true" -> Seq(makeEquation(lhs, rhs))), ("Eq-false" -> Seq(makeInequation(lhs, rhs))))
    CaseDistinction[Defs, Formulae](dist_cases, queryspec, rec_calls, fc_calls, new_global_additional_premises)(obl, obllabels, produce)
  }

}

case class BooleanCaseDistinction[Defs, Formulae <: Defs](body: Formulae,
                                                          queryspec: SpecEnquirer[Defs, Formulae],
                                                          reccalls: Seq[String] = Seq(),
                                                          fc_calls: Seq[String] = Seq(),
                                                          new_global_additional_premises: Seq[Formulae] = Seq()) extends Tactic[Defs, Formulae] {
  import queryspec._

  // TODO: check
  def isApplicable(g: Formulae): Boolean = true

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
  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    //generate a name out of the given body
    val predname = makeFormulaName(body)
    val dist_cases = Map((s"$predname-True" -> Seq(body)), (s"$predname-False" -> Seq(convertExpToNegFormula(body, new_global_additional_premises :+ obl.goal))))
    CaseDistinction[Defs, Formulae](dist_cases, queryspec, reccalls, fc_calls, new_global_additional_premises)(obl, obllabels, produce)
  }

}
