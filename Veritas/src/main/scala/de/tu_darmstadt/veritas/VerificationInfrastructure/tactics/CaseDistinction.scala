package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure._

case class CaseDistinction[Defs, Formulae <: Defs](cases: Seq[Formulae], spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

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
        val added_premises = c +: prems
        //reassemble goal and attach name
        val casename = "-case" + cases.indexOf(c)
        makeNamedFormula(makeForallQuantifyFreeVariables(
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
}

case class StructuralCaseDistinction[Defs, Formulae <: Defs](distvar: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

  import queryspec._

  def isApplicable(g: Formulae): Boolean = isClosedADT(distvar) && isQuantified(g)

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
      val dist_cases_defs_renamed = getCases(distvar) map (c => consolidateFreeVariableNames(c, goalbody))
      val dist_cases = dist_cases_defs_renamed map (dc => makeEquation(distvar, dc))
      CaseDistinction[Defs, Formulae](dist_cases, spec, queryspec)(obl, obllabels, produce)
    } else
      Seq() //TODO throw an exception that explains why the tactic failed
  }
}
