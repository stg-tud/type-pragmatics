package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.CaseDistinction

case class CaseDistinctionStrat[Def, Formulae <: Def](cases: Map[String, Seq[Formulae]], spec_enquirer: SpecEnquirer[Def, Formulae])
  extends Strategy[Def, Formulae] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    val case_tac = CaseDistinction(cases, spec_enquirer)
    pg.applyTactic(obl, case_tac)
    pg
  }
}
