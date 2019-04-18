package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, SortRef}

class RelationalPreservationConstructor(val problem: Problem,
                                        function: FunctionDef,
                                        predicate: FunctionDef,
                                        termIndex: Int,
                                        val hints: Hints) extends LemmaGraphConstructor {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  // --------------------
  // [predicate]([t_1], [t_2])
  // [producer]([], ...) =  []
  // producer arguments can be fresh or bound with matching types
  // the success variable can be any of the arguments of ``predicate``, with matching types
  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  private val inVar = functionArgs(termIndex)
  require(inVar.sortType == termType)
  private val predicateArgs = Seq(inVar, resultVar)

  override def invocationArguments: Seq[MetaVar] = functionArgs

  def generateBase(): AnnotatedLemma = {
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}Preservation${predicate.name}$termIndex", Seq(), Seq(judgment))
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    AnnotatedLemma(lemma, Set(resultVar), Set(resultVar))
  }

  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = {
    node.lemma.boundVariables.filterNot(_.sortType == termType)
  }
}
