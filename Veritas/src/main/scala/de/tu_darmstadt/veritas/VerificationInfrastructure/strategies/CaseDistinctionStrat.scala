package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.CaseDistinction

// expects unnamed cases in correct order
// will attempt to generate appropriate case names (ideally for equations of the form ~mv == ConstructorName(...))
// will append negative pre-patterns to cases according to their order (excluding the previous cases)
case class CaseDistinctionStrat[Def, Formulae <: Def](cases: Seq[Formulae], spec_enquirer: SpecEnquirer[Def, Formulae])
  extends Strategy[Def, Formulae] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    val casemap = for (c <- cases) yield (makeCaseName(c), appendNegativePrepats(cases.take(cases.indexOf(c)), c))


    val case_tac = CaseDistinction(casemap.toMap, spec_enquirer)
    pg.applyTactic(obl, case_tac)
    pg
  }


  private def makeCaseName(f: Formulae): String = spec_enquirer.makeFormulaName(f)


  private def appendNegativePrepats(prepats: Seq[Formulae], f: Formulae): Seq[Formulae] = {
    val negatedprepats = for (pp <- prepats) yield spec_enquirer.convertExpToNegFormula(pp)
    negatedprepats :+ f
  }
}
