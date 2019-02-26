package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplication
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

case class LemmaApplicationStrategy[
Def,
Formulae <: Def,
Type <: Def,
FDef <: Def,
Prop <: Formulae,
Equation <: Def,
Criteria <: Def,
Expression <: Def](override val dsk: DomainSpecificKnowledge[Type, FDef, Prop],
                   override val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression],
                   override val spec_enquirer: SpecEnquirer[Def, Formulae],
                   acg: AugmentedCallGraph[Equation, Criteria, Expression],
                   sel_strat: LemmaSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression],
                   fnames: Seq[String])
  extends DomainSpecificStrategy[Def, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    //first, ensure that fnames does not contain the name of the function for which we are generating the graph
    //(prevent cycles in generated proof graph)
    val acyclic_fnames = fnames.filterNot(fn => acg.toplevel_fun == fn)
    val lemmas: Seq[Formulae] = (for (fn <- acyclic_fnames) yield sel_strat.selectLemma(dsk, acg, fn)).flatten
    if (lemmas.nonEmpty) {
      //only create lemma application node if selection actually yielded lemmas to apply!
      val lemtac = LemmaApplication(lemmas, spec_enquirer)
      pg.applyTactic(obl, lemtac)
    } //otherwise apply solve tactic to ensure complete proof graphs
    else ApplySolve().applyToPG(pg)(obl)

    pg
  }
}
