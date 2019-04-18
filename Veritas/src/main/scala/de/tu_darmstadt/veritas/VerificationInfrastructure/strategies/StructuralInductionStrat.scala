package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction


case class StructuralInductionStrat[Def, Formulae <: Def](distvar: Def, spec_enquirer: SpecEnquirer[Def, Formulae])
  extends Strategy[Def, Formulae] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    val ind_tac = StructuralInduction[Def, Formulae](distvar, spec_enquirer)
    pg.applyTactic(obl, ind_tac)
    pg
  }
}
