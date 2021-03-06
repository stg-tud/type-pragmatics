package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.FunctionCall
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

case class LemmaApplicationAtLeavesStrat[
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
                   sel_strat: LemmaSelectionStrat[Type, FDef, Prop, Equation, Criteria, Expression])
  extends DomainSpecificStrategy[Def, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {
  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    //collect function calls from edges (conservatively throw all of them together)
    val edgelabels = pg.requiringSteps(obl) map (_._2)

    //here we deliberately only look for FunctionCalls and ignore the RecursiveCalls for now
    //these could be used later for provers which you can tell to apply the induction hypothesis at a certain point
    val functioncalls = for (el <- edgelabels; pp <- el.propagateInfoList if pp.isInstanceOf[FunctionCall]) yield pp.asInstanceOf[FunctionCall]
    val fnames = (for (fc <- functioncalls) yield fc.fname).toSeq.distinct

    if (fnames.isEmpty)
      ApplySolve().applyToPG(pg)(obl)
    else
      LemmaApplicationStrat(dsk, acg_gen, spec_enquirer, acg, sel_strat, fnames).applyToPG(pg)(obl)

    pg
  }
}
