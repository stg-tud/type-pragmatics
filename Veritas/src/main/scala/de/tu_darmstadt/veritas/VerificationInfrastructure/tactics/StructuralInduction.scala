package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Verifier, VerifierHints}


case class StructuralInductionHint[Defs, Formulae <: Defs](goal: Formulae, inductionvar: Defs) extends VerifierHints

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
      val iv_cases = getCases(inductionvar, goalbody) map { case (n, ic) =>
        (n, assignCaseVariables(ic, goalbody))
      }

      //recursive arguments of named case terms have to become fixed variables
      val fixed_Vars: Seq[Seq[Defs]] = (iv_cases map (named_ic => getRecArgsADT(named_ic._2))).toSeq


      // form induction subgoals (add equations as premises to the original goal)
      // design decision: all variables become quantified, including fixed ones
      // problem transformation will take care of treating fixed variables accordingly
      val induction_subgoals: Map[String, Formulae] =
      iv_cases map { case (n, named_ic) => {
          val added_premises = makeEquation(inductionvar, named_ic) +: prems
          //reassemble goal and attach name
          val casename = getFormulaName(goal) + n
          casename -> makeNamedGoal(
            makeForallQuantifyFreeVariables(makeImplication(added_premises, concs),
              Seq()), casename)
        }
        }


      // create edge labels: induction hypotheses (all variables universally quantified) + fixed variables!
      val propagatedInfo: Seq[PropagatableInfo] = obtainPropagatableInfo(obllabels)

      val final_ihs: Map[String, StructInductCase[Defs, Formulae]] =
        (for ((fvs, (n, ic)) <- fixed_Vars zip induction_subgoals) yield {
          val casename = getFormulaName(ic)
          if (fvs.isEmpty)
            n -> StructInductCase[Defs, Formulae](casename, Seq(), Seq(), propagatedInfo)
          else {
            val added_premises_ih = for (fv <- fvs) yield makeEquation(inductionvar, fv)
            val ihs = for (ihprem <- added_premises_ih) yield {
              val ihname = casename + "-IH" + added_premises_ih.indexOf(ihprem)
              InductionHypothesis(makeNamedAxiom(makeForall(getUniversallyQuantifiedVars(goal).toSeq,
                makeImplication(ihprem +: prems, concs)), ihname))
            }
            n -> StructInductCase[Defs, Formulae](casename, fvs map (fv => FixedVar(fv)), ihs, propagatedInfo)
          }
        }).toMap

      val finalinductioncases: Map[String, (Formulae, StructInductCase[Defs, Formulae])] = for ((n, f) <- induction_subgoals) yield n -> (f, final_ihs(n))

      //modify the proof graph: create new obligations, return pairs of obligations and edges
      for ((n, (sobl, edge)) <- finalinductioncases) yield {
        val newobl = produce.newObligation(spec, sobl, n)
        (newobl, edge)
      }
    }
    else Seq() //TODO throw an exception that explains why the tactic failed
  }


  /**
    * verifying a step via its edges generates a step result
    * the caller has to decide whether this result will be integrated into a proof graph or not
    *
    * for verifying a step, a tactic may generate a "hint" for the given verifier (depending on the goal and on
    * the given verifier
    *
    * pass hint of structural induction on
    *
    * @param obl
    * @param parentedges
    * @param subobl
    * @param verifier
    * @return
    */
  override def verifyStep[Result <: GenStepResult[Defs, Formulae]](obl: GenObligation[Defs, Formulae],
                                                                   parentedges: Iterable[EdgeLabel],
                                                                   subobl: Iterable[(EdgeLabel, GenObligation[Defs, Formulae])],
                                                                   verifier: Verifier[Defs, Formulae],
                                                                   produce: StepResultProducer[Defs, Formulae, Result],
                                                                   pathforlogs: Option[String]) = {
    val inductionhint = Some(StructuralInductionHint(obl.goal, inductionvar))
    verifier.verify(obl.goal, obl.spec, parentedges, subobl.map { case (e, so) => (e, so.goal) }, inductionhint, produce, pathforlogs)
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
