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
      // restriction: only consider first dispos passed here for the moment! (this is typically correct for structural inductions,
      //but something more sophisticated needs to be done if there has to be a general case distinction at the top-level
    val distvarname: String = acg.getVariableName(acg.getVarExpAtDistarg_pos(Seq(acg_sdroot.arg_exp), distpos.head))
      val distvar: Def = spec_enquirer.makeMVTerm(distvarname)
      // is our function in question a recursive function?
      val funname = acg.toplevel_fun
      val rec_fun = dsk.recursiveFunctions.find { case (fdef, _) => dsk.retrieveFunName(fdef) == funname }

      if (rec_fun.nonEmpty) {
        //do a structural induction
        StructuralInductionStrat(distvar, spec_enquirer).applyToPG(pg)(obl)
      }
      else {
        //check whether original distpos contains more information than used to far
        if (distpos.length == 1)
          //no further distpos information exists, i.e. we can probably just do a structural case distinction
          //this is an approximation and might not work for every function
          StructuralCaseDistinctionStrat(distvar, spec_enquirer).applyToPG(pg)(obl)
        else {
          //we need a general case distinction
          //retrieve cases from root's children
          var rootschildren = acg.getOutgoing(acg_sdroot)
          val sd_children = for (c <- rootschildren if c.isInstanceOf[acg.StructuralDistinction]) yield c.asInstanceOf[acg.StructuralDistinction]
          val rawcases: Seq[Formulae] = for (sd <- sd_children) yield spec_enquirer.convertExpToFormula(sd.arg_exp)
          CaseDistinctionStrat(rawcases, Seq(), spec_enquirer).applyToPG(pg)(obl)
        }
      }
    } else {
      //check whether the root node calls functions
      val root_fcparents = acg.getFCParents(acg_sdroot)
      if (root_fcparents.nonEmpty) {
        //collect lemmas for the individual function calls via lemma selection strategy and apply a lemma application
        LemmaApplicationStrategy(dsk, acg_gen, spec_enquirer, acg, SelectAllSelectionStrategy(), root_fcparents.map { case acg.FunctionCall(_, _, fn) => fn }).applyToPG(pg)(obl)
      }
    }


    def isLeafNodeWithoutFCParent(n: acg.Node): Boolean = (acg.leaves contains n) && acg.getFCParents(n).isEmpty

    //retrieve an obligation from a list of given obligations that fits to the given ACG_node
    // there are probably some gaps in this function, needs to be refined for concrete cases
    def getMatchingObl(acg_node: acg.Node, obls: Seq[pg.Obligation]): Option[pg.Obligation] = {
      val goalmap: Map[String, pg.Obligation] = (obls map (o => (spec_enquirer.getFormulaName(o.goal) -> o))).toMap

      var accumulated_namestrings: Seq[String] = Seq() //for debugging of namepred construction

      //helper function for calculating part of a name that an obligation could have, from the distinguishing parts in ACG nodes
      def getNamePred(acgn: acg.Node): String => Boolean =
        acgn match {
          case acg.StructuralDistinction(_, arg_exp, _) => {
            //attempt to find the name for the case that the spec_enquirer would generate as substring of a goalname
            val farg_exp = spec_enquirer.convertExpToFormula(arg_exp)
            val genname = spec_enquirer.makeFormulaName(farg_exp)
            accumulated_namestrings = accumulated_namestrings :+ genname
            (n: String) => n.contains(accumulated_namestrings.reverse.mkString("-"))
          }
          case acg.BooleanDistinction(_, _, criteria, _, _) => {
            val f = spec_enquirer.convertExpToFormula(criteria)
            val predname = spec_enquirer.makeFormulaName(f)
            if (spec_enquirer.isNegation(f)) {
              //look for "false" in goalnames
              accumulated_namestrings = accumulated_namestrings :+ predname + "-False"
              (n: String) => n.contains(accumulated_namestrings.reverse.mkString("-"))
            }
            else {
              //look for "true" in goalnames
              accumulated_namestrings = accumulated_namestrings :+ predname +  "-True"
              (n: String) => n.contains(accumulated_namestrings.reverse.mkString("-"))
            }
          }
        }

      // refine the search for an appropriate problem name in obls until a single problem name is found
      var selected_goalnames: Seq[String] = Seq()
      var next_acg_node = Seq(acg_node)
      var namepred: String => Boolean = getNamePred(acg_node)

      while (selected_goalnames.length != 1 && next_acg_node.length == 1) {
        selected_goalnames = (goalmap.keys.filter(namepred)).toSeq

        //go up in acg for next iteration
        next_acg_node = acg.getParents(next_acg_node.head).filterNot(p => p.isInstanceOf[acg.FunctionCall]) //do not consider function call parents
        //refine namepred according to next_acg_node
        if (next_acg_node.nonEmpty) {
          namepred = getNamePred(next_acg_node.head)
        }
      }


      if (selected_goalnames.length == 1)
        Some(goalmap(selected_goalnames.head))
      else {
        println(s"Could not find a unique matching obligation for acg_node $acg_node in $obls")
        None
      }
    }


    def makebindingFormula(varname: String, rhs: Expression): Formulae =
      spec_enquirer.makeEquation(spec_enquirer.makeMVTerm(varname), rhs)


    // 2) iteratively traverse given acg and grow proof graph according to ACG information

    //loop initialization:
    // a) get children from root node; ignore leaves without any function call parents (will be treated at the end)
    var curr_acg_nodes: Seq[acg.Node] = acg.getOutgoing(acg_sdroot).filterNot(isLeafNodeWithoutFCParent)
    // b) get intermediate leaves from current proof graph (previously generated (induction) case obligations)
    var currobls: Seq[pg.Obligation] = pg.leaves(Set(obl))
    var visitednodes: Set[acg.Node] = Set(acg_sdroot) //will not contain leaves; we treat leaves together at the end - do we need to keep track of visited nodes? ACGs are acyclic?

    //simple lemma selection strategy for now, refine later
    val sel_strat = SelectAllSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression]()

    //apply tactics to corresponding obligations according to information in acg
    while (curr_acg_nodes.nonEmpty) {
      var nextcurrnodes: Seq[acg.Node] = Seq()
      var nextcurrobls: Seq[pg.Obligation] = Seq()
      for (cn <- curr_acg_nodes) {
        //retrieve the obligation from the proof graph that corresponds to the current acg node
        //the current obligation currobl is the obligation in the proof graph that we have to apply a tactic on
        val maybecurrobl = getMatchingObl(cn, currobls) match {
          case Some(currobl) => {
            //retrieve all direct parents that are function calls
            var fc_parents = acg.getFCParents(cn)

            //retrieve all relevant children (ignore visited nodes and leaf nodes without function call parents)
            // result should only be StructuralDistinctions and BooleanDistinctions
            var children = acg.getOutgoing(cn)
            val sd_children = for (c <- children if c.isInstanceOf[acg.StructuralDistinction]) yield c.asInstanceOf[acg.StructuralDistinction]
            val bd_children = for (c <- children if c.isInstanceOf[acg.BooleanDistinction]) yield c.asInstanceOf[acg.BooleanDistinction]

            //case 1) There are only structural distinction children.
            if (fc_parents.isEmpty && children.nonEmpty && (sd_children.length == children.length)) {
              val rawcases: Seq[Formulae] = for (sd <- sd_children) yield spec_enquirer.convertExpToFormula(sd.arg_exp)
              CaseDistinctionStrat(rawcases, Seq(), spec_enquirer).applyToPG(pg)(currobl)
            }
            // case 2) There are only function call parents -> try lemma application
            else if (fc_parents.nonEmpty && children.isEmpty) {
              //make sure to exclude recursive calls
              val fcnames = (fc_parents map (_.name)).filterNot(fn => acg.toplevel_fun == fn)

              LemmaApplicationStrategy(dsk, acg_gen, spec_enquirer, acg, sel_strat, fcnames).applyToPG(pg)(currobl)
            }
            // case 3) There are exactly two Boolean distinction children -> apply Boolean distinction
            // first child contains the positive condition
            else if (bd_children.length == 2) {
              val poscond: Formulae = spec_enquirer.convertExpToFormula(bd_children.head.criteria)
              //both children have to contain the same new bindings
              val binding_premises: Seq[Formulae] = (for ((n, exp) <- bd_children.head.new_bindings) yield makebindingFormula(n, exp)).toSeq

              // if there are also function call parents present: propagate the calls found through the proof graph along proof edges
              // will result in lemma applications at the leaves later
              val funcalls = if (fc_parents.isEmpty) Seq() else for (fcp <- fc_parents) yield fcp.name

              //partition funcalls into recursive funcall and all others; omit duplicate names if any
              val (reccalls, othercalls) = funcalls.distinct.partition(s => acg.toplevel_fun == s)

              BooleanCaseDistinctionStrat[Def, Formulae](poscond, spec_enquirer, reccalls, othercalls, binding_premises).applyToPG(pg)(currobl)
            }
            //ignore all other cases - in all other cases, ACG is not well-constructed.


            //update currnodes and visitednodes for next iteration of outer loop
            visitednodes += cn
            //we do not need to inspect FunctionCall nodes further
            //also, we do not need to inspect leaf nodes without FC parents further - these will be treated in the final step.
            nextcurrnodes ++= children.filterNot(c => (visitednodes contains c) || isLeafNodeWithoutFCParent(c))
            nextcurrobls ++= pg.leaves(Set(currobl))
          }
          case None => //do nothing (will exit while loop then, with potentially incomplete proof graph)
        }
      }

      curr_acg_nodes = nextcurrnodes
      currobls = nextcurrobls
    }

    //3) final step: Treat leaves. Either require a lemma application or application of Solve tactic (depending on proof edges)
    new ApplyStratToLeaves(LemmaApplicationAtLeavesStrat(dsk, acg_gen, spec_enquirer, acg, sel_strat)).applyToPG(pg)(obl)

    pg
  }


}
