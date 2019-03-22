package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplication
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * iterative strategy - builds a proof graph according to the "template" given by the augmented call graph
  * assumes a proof graph that already contains nodes (at least root nodes)
  *
  * @param dsk     domain-specific knowledge for the given specification
  * @param acg_gen function that can generate an augmented call graph for a given function name
  */
case class ProgressPreservationBasicLoop[
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
                   retrievePropFromGoal: Formulae => Prop)
  extends DomainSpecificStrategy[Def, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {


  // given obligation is the obligation to which the basic loop shall be applied
  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation):
  ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {

    //step0: save current leaves for checking generation progress
    val old_leaves = pg.leaves(Set(obl))

    //step 1: retrieve relevant function name from given obligation with the help of the given domain specific knowledge dsk
    val goal = obl.goal
    val prop = retrievePropFromGoal(goal)

    val retrievefdef = findFDefForDSKProp(prop)

    val fname = retrievefdef match {
      case Some(fdef) => dsk.retrieveFunName(fdef)
      case None => sys.error(s"Basic Loop for progress/preservation proof generation could not retrieve a corresponding function for given goal $obl")
    }

    //step 2: create augmented call graph for relevant function
    val acg = acg_gen(fname)

    //step 3: apply strategy that traverses the computed ACG and grows the given proof graph accordingly
    GenerateSubgraphForSingleFunction(dsk, acg_gen, spec_enquirer, acg).applyToPG(pg)(obl)


    //step 4: Collect all leaves that contain auxiliary lemmas (i.e. are children of a lemma application)
    val new_leaves = pg.leaves(Set(obl))

    def isLemmaApplicationLeaf(l: pg.Obligation): Boolean = {
      val maybe_lemtac = pg.requiringSteps(l).find { case (ps, _) => ps.tactic.isInstanceOf[LemmaApplication[Def, Formulae]] }
      maybe_lemtac.nonEmpty
    }

    //step 5: recall the basic loop for all new leaves collected above, if there was progress compared to previous leaves
    if (new_leaves != old_leaves) {
      val pp_leaves: Seq[pg.Obligation] = for (l <- new_leaves if isLemmaApplicationLeaf(l)) yield l

      for (l <- pp_leaves) this.applyToPG(pg)(l)
    }


    pg
  }

  private def findFDefForDSKProp(p: Prop): Option[FDef] = {
    val preservationFDef = dsk.preservationProperties.find(pair => pair._2 contains p).map(_._1)
    val progressFDef = dsk.progressProperties.find(pair => pair._2 contains p).map(_._1)
    val otherFDef = dsk.auxiliaryProperties.find(pair => pair._2 contains p).map(_._1)

    if (preservationFDef.nonEmpty) preservationFDef else if (progressFDef.nonEmpty) progressFDef else otherFDef
  }
}
