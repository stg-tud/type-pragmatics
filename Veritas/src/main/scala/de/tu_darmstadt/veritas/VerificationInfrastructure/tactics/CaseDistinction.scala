package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure._



case class CaseDistinction[Defs, Formulae <: Defs](cases: Seq[Seq[Formulae]], spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

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
      cases map { c => {
        //simply add each goal to the premises
        val added_premises = c ++ prems
        //reassemble goal and attach name
        val casename = "-case" + cases.indexOf(c)
        makeNamedGoal(makeForallQuantifyFreeVariables(
          makeImplication(added_premises, concs)), getFormulaName(goal) ++ casename)
      }
      }

    val propagatedInfo: Seq[PropagatableInfo] = obtainPropagatableInfo(obllabels)

    val edgelabels = for (cs <- case_subgoals) yield
      CaseDistinctionCase(getFormulaName(cs), propagatedInfo)

    for ((caseobl, edge) <- case_subgoals zip edgelabels) yield {
      val newobl = produce.newObligation(spec, caseobl)
      (newobl, edge)
    }
  }

  //call with a parent obligation and the sub-obligations that this parent requires
  //will try to match the cases against induction cases and then assemble a map from case names to (Obligation, EdgeLabel)
  def enumerateCases[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Map[String, (Obligation, EdgeLabel)] = {
    (for (r <- required) yield {
      r._2 match {
        case CaseDistinctionCase(name, _) => name -> r
        case c => sys.error(s"Enumerate cases of a case distinction: The given required sub-obligations were not all labeled as case distinction cases: $c")
      }
    }).toMap
  }

  def enumerateCaseNames[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Seq[String] =
    enumerateCases(required).keys.toSeq.sortWith(_ < _) //alphabetical ordering

  def selectCase[Obligation](casename: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
    enumerateCases(required)(casename)._1
}

case class StructuralCaseDistinction[Defs, Formulae <: Defs](distvar: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

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
      val dist_cases_defs_renamed = getCases(distvar, goalbody) map (c => assignCaseVariables(c, goalbody))
      val dist_cases = dist_cases_defs_renamed map (dc => Seq(makeEquation(distvar, dc)))
      CaseDistinction[Defs, Formulae](dist_cases, spec, queryspec)(obl, obllabels, produce)
    } else
      Seq() //TODO throw an exception that explains why the tactic failed
  }
}

case class EqualityCaseDistinction[Defs, Formulae <: Defs](lhs: Defs, rhs: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {
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

    val dist_cases = Seq(Seq(makeEquation(lhs, rhs)), Seq(makeInequation(lhs, rhs)))
    CaseDistinction[Defs, Formulae](dist_cases, spec, queryspec)(obl, obllabels, produce)
  }

}

case class BooleanCaseDistinction[Defs, Formulae <: Defs](body: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {
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

    val dist_cases = Seq(Seq(makeTypingFunctionExpression(body)), Seq(makeNegation(body)))
    CaseDistinction[Defs, Formulae](dist_cases, spec, queryspec)(obl, obllabels, produce)
  }

}
