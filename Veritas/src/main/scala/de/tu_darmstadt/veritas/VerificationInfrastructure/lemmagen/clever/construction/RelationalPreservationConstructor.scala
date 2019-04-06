package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, SortRef}

/** Refinement graph constructor for relational preservation lemmas. */
class RelationalPreservationConstructor(val problem: Problem,
                                        function: FunctionDef,
                                        relation: FunctionDef,
                                        argIndex: Int,
                                        val hints: Hints) extends LemmaGraphConstructor {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  // generate fresh variable symbols for the invocation of `function`, as well as for its result
  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  // retrieve the i-th argument with i = argIndex
  private val inVar = functionArgs(argIndex)
  require(inVar.sortType == termType)
  // build arguments for the relation in consequence
  private val relationArgs = Seq(inVar, resultVar)

  override def invocationArguments: Seq[MetaVar] = functionArgs

  def generateBase(): AnnotatedLemma = {
    // build a lemma whose consequence contains an invocation of `relation`
    val invocationExp = FunctionExpApp(relation.name, Assignments.wrapMetaVars(relationArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}Preservation${relation.name}$argIndex", Seq(), Seq(judgment))
    // add a successful invocation of `function`
    val lemma =  Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
      .refine(problem, baseLemma).getOrElse(baseLemma)
    AnnotatedLemma(lemma, Set(resultVar), Set(resultVar))
  }

  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = {
    node.lemma.boundVariables.filterNot(_.sortType == termType)
  }
}
