package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{FunctionCall, LemmaApplication}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

case class LemmaApplicationStrat[
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
                   sel_strat: LemmaSelectionStrat[Type, FDef, Prop, Equation, Criteria, Expression],
                   fnames: Seq[String])
  extends DomainSpecificStrategy[Def, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    //collect propagated function calls from edges (conservatively throw all of them together) and add them to fnames
    val edgelabels = pg.requiringSteps(obl) map (_._2)

    //here we deliberately only look for FunctionCalls and ignore the RecursiveCalls for now
    //these could be used later for provers which you can tell to apply the induction hypothesis at a certain point
    val functioncalls = for (el <- edgelabels; pp <- el.propagateInfoList if pp.isInstanceOf[FunctionCall]) yield pp.asInstanceOf[FunctionCall]
    val aug_fnames = ((for (fc <- functioncalls) yield fc.fname).toSeq ++ fnames).distinct


    val lemmas: Seq[Formulae] = (for (fn <- aug_fnames) yield sel_strat.selectLemma(dsk, acg, fn)).flatten
    if (lemmas.nonEmpty) {
      //only create lemma application node if selection actually yielded lemmas to apply!
      val lemtac = LemmaApplication(lemmas, spec_enquirer)
      pg.applyTactic(obl, lemtac)
    } //otherwise apply solve tactic to ensure complete proof graphs
    else ApplySolve().applyToPG(pg)(obl)

    pg
  }
}
