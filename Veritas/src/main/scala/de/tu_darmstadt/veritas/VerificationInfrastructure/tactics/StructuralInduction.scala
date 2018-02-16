package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer

case class StructuralInduction[Defs, Formulae <: Defs](inductionvar: Defs, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

  import queryspec._

  //TODO isApplicable is a candidate for a common Tactic method
  /**
    * determines whether this tactic is applicable to a given goal
    *
    *
    * structural induction:
    * - goal has pattern "forall v1 iv v2. premises => conclusions (where v1, v2 are lists of variables,
    * and premises/conclusions are conjuncts of expressions; premises may be the empty list, conclusions must not be empty)
    * - induction variable is of type "closed ADT"
    * - premises or conclusions of goal contain calls to recursive functions/jugdments
    */
  def isApplicable(g: Formulae): Boolean = {
    val fcs = extractFunctionCalls(g)
    //note: line below will currently only work if there is at least one function call (to a recursive function)
    // that has the induction variable directly as argument
    val rec_functioncall_with_inductionvar: Option[Defs] = fcs find
      (fc => isRecursiveFunctionCall(fc) &&
        (getRecursiveArguments(fc) contains inductionvar))
    goalMatchesPattern(g) &&
      (getUniversallyQuantifiedVars(g) contains inductionvar) &&
      isClosedADT(inductionvar, g) &&
      rec_functioncall_with_inductionvar.isDefined
    //TODO: refine conditions, if necessary
  }

  private def getRecursiveArguments(call: Defs): Seq[Defs] = {
    val args = getArguments(call)
    args.flatMap { arg =>
      val functionCalls = extractFunctionCalls(arg)
      if (functionCalls.isEmpty)
        arg +: getArguments(arg)
      else
        functionCalls.flatMap(getRecursiveArguments)
    }
  }

  //TODO goalMatchesPattern is a candidate for a common Tactic method
  // idea: introduce a small pattern language to describe goal patterns?
  def goalMatchesPattern(g: Formulae): Boolean =
    isForall(g) && {
      val body = getQuantifiedBody(g)
      isImplication(body) && getConclusions(body).nonEmpty
    }


  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]):
  Iterable[(Obligation, EdgeLabel)] = {
    val goal = obl.goal
    if (isApplicable(goal)) {
      val goalbody = getQuantifiedBody(goal)
      val prems = getPremises(goalbody)
      val concs = getConclusions(goalbody)

      //get terms for individual ADT cases,
      // make sure that variables in cases terms do not clash with any variables in body of goal
      val iv_cases = getCases(inductionvar, goalbody) map { ic =>
        assignCaseVariables(ic, goalbody)
      }

      //recursive arguments of named case terms have to become fixed variables
      val fixed_Vars: Seq[Seq[Defs]] = iv_cases map (named_ic => getRecArgsADT(named_ic))


      // form induction subgoals (add equations as premises to the original goal)
      // design decision: all variables become quantified, including fixed ones
      // problem transformation will take care of treating fixed variables accordingly
      val induction_subgoals: Seq[Formulae] =
        iv_cases map { named_ic => {
          val added_premises = makeEquation(inductionvar, named_ic) +: prems
          //reassemble goal and attach name
          val casename = "-icase" + iv_cases.indexOf(named_ic)
          makeNamedGoal(
            makeForallQuantifyFreeVariables(makeImplication(added_premises, concs),
              Seq()), getFormulaName(goal) ++ casename)
        }
        }


      // create edge labels: induction hypotheses (all variables universally quantified) + fixed variables!
      val propagatedInfo: Seq[PropagatableInfo] = obtainPropagatableInfo(obllabels)

      val final_ihs: Seq[StructInductCase[Defs, Formulae]] =
        for ((fvs, ic) <- fixed_Vars zip induction_subgoals) yield {
          val casename = getFormulaName(ic)
          if (fvs.isEmpty)
            StructInductCase[Defs, Formulae](casename, Seq(), Seq(), propagatedInfo)
          else {
            val added_premises_ih = for (fv <- fvs) yield makeEquation(inductionvar, fv)
            val ihs = for (ihprem <- added_premises_ih) yield {
              val ihname = casename + "-IH" + added_premises_ih.indexOf(ihprem)
              InductionHypothesis(makeNamedAxiom(makeForall(getUniversallyQuantifiedVars(goal).toSeq,
                makeImplication(ihprem +: prems, concs)), ihname))
            }
            StructInductCase[Defs, Formulae](casename, fvs map (fv => FixedVar(fv)), ihs, propagatedInfo)
          }
        }

      val finalinductioncases: Seq[(Formulae, StructInductCase[Defs, Formulae])] = induction_subgoals zip final_ihs

      //modify the proof graph: create new obligations, return pairs of obligations and edges
      for ((sobl, edge) <- finalinductioncases) yield {
        val newobl = produce.newObligation(spec, sobl)
        (newobl, edge)
      }
    }
    else Seq() //TODO throw an exception that explains why the tactic failed
  }

  //call with a parent obligation and the sub-obligations that this parent requires
  //will try to match the cases against induction cases and then assemble a map from case names to (Obligation, EdgeLabel)
  def enumerateCases[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Map[String, (Obligation, EdgeLabel)] = {
    (for (r <- required) yield {
      r._2 match {
        case StructInductCase(name, _, _, _) => name -> r
        case c => sys.error(s"Enumerate cases of a structural induction: The given required sub-obligations were not all labeled as induction cases: $c")
      }
    }).toMap
  }

  def enumerateCaseNames[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Seq[String] =
    enumerateCases(required).keys.toSeq.sortWith(_ < _) //alphabetical ordering

  def selectCase[Obligation](casename: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
    enumerateCases(required)(casename)._1

}
