package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.BooleanCaseDistinction

case class BooleanCaseDistinctionStrat[Def, Formulae <: Def](poscond: Formulae, spec_enquirer: SpecEnquirer[Def, Formulae], additional_premises: Seq[Formulae] = Seq())
  extends Strategy[Def, Formulae] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    val bcase_tac = BooleanCaseDistinction[Def, Formulae](poscond, spec_enquirer, additional_premises)
    pg.applyTactic(obl, bcase_tac)
    pg
  }
}