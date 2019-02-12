package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * traverses a given augmented call graph for a function and grows the given proof graph at the given obligation
  * assumes that the given augmented call graph "fits" to the goal in the given obligation
  * also receives the function for creating new augmented call graphs, since it implements DomainSpecificStrategy
  * currently, this would not be needed, but might be interesting for future refined strategies
  *
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

case class GenerateSubgraphForSingleFunction[
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
                   acg: AugmentedCallGraph[Equation, Criteria, Expression])
  extends DomainSpecificStrategy[Def, Formulae, Type, FDef, Prop, Equation, Criteria, Expression] {

  override def applyToPG(pg: ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae])(obl: pg.Obligation): ProofGraph[Def, Formulae] with ProofGraphTraversals[Def, Formulae] = {
    val acg_sdroot = acg.sdroots.head //we implicitly assume that graph has only one structural distinction node as root
    val toplevel_call = acg_sdroot.arg_exp //this is the expression to which all the argument position

    //1) treat root node of acg separately, since it is the only node which may induce a structural induction step
    if (acg_sdroot.arg_pos.nonEmpty) { //first check whether we have to do a distinction at all
      val distpos = acg_sdroot.arg_pos.get //this position is either the argument position in which the function is recursive or in which the top-level cases can be distinguished
    val distvarname: String = acg.getVariableName(acg.getVarExpAtDistarg_pos(Seq(acg_sdroot.arg_exp), distpos))
      val distvar: Def = spec_enquirer.makeMVTerm(distvarname)
      // is our function in question a recursive function?
      val funname = acg.toplevel_fun
      val rec_fun = dsk.recursiveFunctions.find { case (fdef, _) => dsk.retrieveFunName(fdef) == funname }

      if (rec_fun.nonEmpty) {
        //do a structural induction
        StructuralInductionStrat(distvar, spec_enquirer).applyToPG(pg)(obl)
      }
      else {
        //do a structural case distinction for the distinction var
        //this is an approximation and might not work for every function
        //TODO: analyze cases and do a general case distinction if necessary?
        StructuralCaseDistinctionStrat(distvar, spec_enquirer).applyToPG(pg)(obl)
      }
    } else {
      //check whether the root node calls functions
      val root_fcparents = acg.getFCParents(acg_sdroot)
      if (root_fcparents.nonEmpty) {
        //collect lemmas for the individual function calls via lemma selection strategy and apply a lemma application
        LemmaApplicationStrategy(dsk, acg_gen, spec_enquirer, acg, SimplyFirstSelectionStrategy(), root_fcparents.map { case acg.FunctionCall(_, _, fn) => fn })
      }
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

        //retrieve all direct parents that are function calls
        var fc_parents = acg.getFCParents(cn)

        //retrieve all relevant children (ignore visited nodes and leaf nodes without function call parents)
        // result should only be StructuralDistinctions and BooleanDistinctions
        var children = acg.getOutgoing(cn).filterNot(c => (visitednodes contains c) || isLeafNodeWithoutFCParent(c))
        val sd_children = for (c <- children if c.isInstanceOf[acg.StructuralDistinction]) yield c.asInstanceOf[acg.StructuralDistinction]
        val bd_children = for (c <- children if c.isInstanceOf[acg.BooleanDistinction]) yield c.asInstanceOf[acg.BooleanDistinction]

        //case 1) There are only structural distinction children.
        if (fc_parents.isEmpty && children.nonEmpty && (sd_children.length == children.length)) {
          val rawcases: Seq[Formulae] = for (sd <- sd_children) yield spec_enquirer.convertExpToFormula(sd.arg_exp)
          CaseDistinctionStrat(rawcases, spec_enquirer).applyToPG(pg)(currobl)
        }
        // case 2) There are only function call parents -> try lemma application
        else if (fc_parents.nonEmpty && children.isEmpty) {
          val fcnames = fc_parents map (_.name)
          val sel_strat = SimplyFirstSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression]() //simple selection strategy for now, refine later
          LemmaApplicationStrategy(dsk, acg_gen, spec_enquirer, acg, sel_strat, fcnames).applyToPG(pg)(currobl)
        }
        // case 3) There are exactly two Boolean distinction children -> apply Boolean distinction
        else if (fc_parents.isEmpty && bd_children.length == 2) {
          val poscond: Formulae = spec_enquirer.convertExpToFormula(bd_children.head.criteria)
          BooleanCaseDistinctionStrat[Def, Formulae](poscond, spec_enquirer)
        }
        //case 4) There are function call parents AND 2 Boolean distinction children
        else if (fc_parents.nonEmpty && bd_children.length == 2) {
          //TODO Apply a BooleanDistinction AND propagate the names of function calls for which we need lemma applications
        }
        //ignore all other cases - in all other cases, ACG is not well-constructed.


        //update currnodes and visitednodes for next iteration of outer loop
        visitednodes += cn
        nextcurrnodes ++= children //we do not need to inspect FunctionCall nodes further
        nextcurrobls ++= pg.leaves(Set(currobl))

      }

      curr_acg_nodes = nextcurrnodes
      currobls = nextcurrobls
    }

    //3) final step: apply Solve tactic to all remaining leaves in graph
    ApplySolveToLeaves().applyToPG(pg)(obl)

    pg
  }


}
