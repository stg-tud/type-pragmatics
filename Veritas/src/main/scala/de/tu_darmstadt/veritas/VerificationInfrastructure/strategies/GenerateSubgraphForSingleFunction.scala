package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * traverses a given augmented call graph for a function and grows the given proof graph at the given obligation
  * assumes that the given augmented call graph "fits" to the goal in the given obligation
  * also receives the function for creating new augmented call graphs, since it implements DomainSpecificStrategy
  * currently, this would not be needed, but might be interesting for future refined strategies
  * @param dsk
  * @param acg_gen
  * @param acg
  * @tparam Spec
  * @tparam Goal
  * @tparam Type
  * @tparam FDef
  * @tparam Prop
  * @tparam Equation
  * @tparam Criteria
  * @tparam Expression
  */

case class GenerateSubgraphForSingleFunction[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression] (override val dsk: DomainSpecificKnowledge[Type, FDef, Prop],
                                                                                                            override val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression],
                                                                                                            acg: AugmentedCallGraph[Equation, Criteria, Expression])
  extends DomainSpecificStrategy[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression] {

  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    val acg_sdroot = acg.sdroots.head //we implicitly assume that graph has only one structural distinction node as root

    //1) treat root node of acg separately, since it is the only node which may induce a structural induction step
    if (acg_sdroot.arg_pos.nonEmpty) { //first check whether we have to do a distinction at all
      // is our function in question a recursive function?
      val funname = acg.toplevel_fun
      val rec_fun = dsk.recursiveFunctions.find { case (fdef, _) => dsk.retrieveFunName(fdef) == funname }
      if (rec_fun.nonEmpty) {
        //retrieve the recursive position
        val recpos = rec_fun.get._2._2 //maybe rather take from ACG-node and not from DSK?? No, by design this has to be the SAME position.
        ??? //TODO do structural induction
      }
      else
        ??? //TODO do structural case distinction
    } else {
      //check whether the root node calls functions
      val root_fcparents = acg.getFCParents(acg_sdroot)
      if (root_fcparents.nonEmpty)
        ??? //TODO do lemma application
    }


    def isLeafNodeWithoutFCParent(n: acg.Node): Boolean = (acg.leaves contains n) && acg.getFCParents(n).isEmpty

    //retrieve an obligation from a list of given obligations that fits to the given ACG_node
    def getMatchingObl(acg_node: acg.Node, obls: Seq[pg.Obligation]): pg.Obligation = ??? //TODO How to do that abstractly? Via goal names?

    // 2) iteratively traverse given acg

    //loop initialization:
    // a) get children from root node; ignore leaves without any function call parents (will be treated at the end)
    var curr_acg_nodes: Seq[acg.Node] = acg.getOutgoing(acg_sdroot).filterNot(isLeafNodeWithoutFCParent)
    // b) get intermediate leaves from current proof graph (previously generated (induction) case obligations)
    var currobls: Seq[pg.Obligation] = pg.leaves(Set(obl))
    var visitednodes: Set[acg.Node] = Set(acg_sdroot) //will not contain leaves; we treat leaves together at the end - do we need to keep track of visited nodes? ACGs are acyclic?

    //apply tactics to corresponding obligations according to information in acg
    while (curr_acg_nodes.nonEmpty) {
      var nextcurrnodes: Seq[acg.Node] = Seq()
      var nextcurrobls: Seq[pg.Obligation] = Seq()
      for (cn <- curr_acg_nodes) {
        //retrieve the obligation from the proof graph that corresponds to the current acg node
        //the current obligation currobl is the obligation in the proof graph that we have to apply a tactic on
        val currobl = getMatchingObl(cn, currobls)

        // TODO decide which tactic has to be applied to currobl based on cn's parents/children

        //retrieve all direct parents that are function calls
        var fc_parents = acg.getFCParents(cn)

        //retrieve all relevant children (ignore visited nodes and leaf nodes without function call parents)
        // result should only be StructuralDistinctions and BooleanDistinctions
        var children = acg.getOutgoing(cn).filterNot(c => (visitednodes contains c) || isLeafNodeWithoutFCParent(c) )

        //update currnodes and visitednodes for next iteration of outer loop
        visitednodes += cn
        nextcurrnodes ++= children //we do not need to inspect FunctionCall nodes further
        nextcurrobls ++= pg.leaves(Set(currobl))

      }

      curr_acg_nodes = nextcurrnodes
      currobls = nextcurrobls
    }

    //3) final step: apply Solve tactic to all leaves in graph
    ApplySolveToLeaves().applyToPG(pg)(obl)

    pg
  }



}
