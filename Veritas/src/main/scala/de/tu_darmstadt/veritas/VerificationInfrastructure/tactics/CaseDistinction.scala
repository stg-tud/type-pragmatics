package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, IProofGraph, ObligationProducer}

case class CaseDistinction[Defs <: Ordered[Defs], Formulae <: Defs with Ordered[Formulae]](distvar: Defs, cases: Seq[Formulae], spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

  import queryspec._
  // tactic is always applicable (no applicability condition)?
  // assume that goal is a quantified expression


  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    val goal = obl.goal
    val goalbody = getQuantifiedBody(goal)
    val prems = getPremises(goalbody) //could be empty
    val concs = getConclusions(goalbody)
    val renamed_cases = cases map (c => consolidateFreeVariableNames(c, goalbody))

    val case_subgoals: Seq[Formulae] =
      renamed_cases map { case (renamed_c, new_quantified_vars) => {
        // add equations for different cases of distvar as premises
        val added_premises = makeEquation(distvar, renamed_c) +: prems
        val added_quantvars = new_quantified_vars ++ getUniversallyQuantifiedVars(goal)
        //reassemble goal and attach name
        val casename = "-case" + renamed_cases.indexOf((renamed_c, new_quantified_vars))
        makeNamedFormula(makeForall(added_quantvars,
          makeImplication(added_premises, concs)), getFormulaName(goal) ++ casename)
      }}

    //TODO: are fixed variables required in case distinctions?
    // not new ones, but fixed ones propagated in the obllabels have to be forwarded!

    val edgelabels = for (cs <- case_subgoals) yield {
      val casename = getFormulaName(cs)
      //TODO: propagate information from edges properly....! (e.g. nested induction)
      CaseDistinctionCase(casename, ???, ???)
    }

    for ((caseobl, edge) <- case_subgoals zip edgelabels) yield {
      val newobl = produce.newObligation(spec, caseobl)
      (newobl, edge)
    }



  }

  override def compare(that: Tactic[Defs, Formulae]): Int = that match {
    case that: CaseDistinction[Defs, Formulae] => {
      val lengthcomp = this.cases.length compare that.cases.length
      if (lengthcomp == 0) {
        lazy val comp_individual_cases = (this.cases zip that.cases) map { case (c1, c2) => c1 compare c2 }
        (comp_individual_cases find (_ != 0)).getOrElse(0)
      }
      else lengthcomp
    }
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

}

case class StructuralCaseDistinction[Defs <: Ordered[Defs], Formulae <: Defs with Ordered[Formulae]](distvar: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {


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
                                 produce: ObligationProducer[Defs, Formulae, Obligation]) =
    ???

  override def compare(that: Tactic[Defs, Formulae]) = ???
}
