package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, NotJudgment, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable

class RelationalPreservationConstructor(val problem: Problem,
                                       function: FunctionDef,
                                       predicate: FunctionDef,
                                       hints: Option[Hints]) extends LemmaGraphConstructor {
  import Query._
  implicit private val enquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head // TODO
  }

  // --------------------
  // [predicate]([t_1], [t_2])
  // [producer]([], ...) =  []
  // producer arguments can be fresh or bound with matching types
  // the success variable can be any of the arguments of ``predicate``, with matching types
  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  private val matchingInVars = functionArgs.filter(_.sortType == termType)
  require(matchingInVars.size == 1)
  private val inVar = matchingInVars.head
  private val relationConstraints = Seq(Constraint.fixed(inVar), Constraint.fixed(resultVar))
  private val predicateArgs = Assignments.generate(relationConstraints).head

  override def invocationArguments: Seq[MetaVar] = functionArgs

  def generateBase(): AnnotatedLemma = {
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we find all inVars with matching type
    //val matchingInVars = inVars.filter(_.sortType == outType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    AnnotatedLemma(lemma, predicateArgs.toSet, Set(resultVar))
  }

  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = {
    node.lemma.boundVariables.filterNot(_.sortType == termType)
  }

  override def constructRoot(): AnnotatedLemma = {
    val base = generateBase()
    hints match {
      case None => base
      case Some(actualHints) => actualHints.apply(base)
    }
  }

}
