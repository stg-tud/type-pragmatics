package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplication
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

case class LemmaApplicationStrategy[Defs, Formulae <: Defs, Type, FDef, Prop <: Formulae, Equation, Criteria, Expression](override val dsk: DomainSpecificKnowledge[Type, FDef, Prop],
                                                                                                              override val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression],
                                                                                                              override val spec_enquirer: SpecEnquirer[Defs, Formulae],
                                                                                                              acg: AugmentedCallGraph[Equation, Criteria, Expression],
                                                                                                              sel_strat: LemmaSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression],
                                                                                                              fnames: Seq[String])
  extends DomainSpecificStrategy[Defs, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {

  override def applyToPG(pg: ProofGraph[Defs, Formulae] with ProofGraphTraversals[Defs, Formulae])(obl: pg.Obligation): ProofGraph[Defs, Formulae] with ProofGraphTraversals[Defs, Formulae] = {
    val lemmas: Seq[Formulae] = (for (fn <- fnames) yield sel_strat.selectLemma(dsk, acg, fn)).flatten
    val lemtac = LemmaApplication(lemmas, spec_enquirer)
    pg.applyTactic(obl, lemtac)

    pg
  }
}
