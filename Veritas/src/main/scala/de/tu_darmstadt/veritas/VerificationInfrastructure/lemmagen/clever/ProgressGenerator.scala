package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.backend.ast.{ExistsJudgment, FunctionExpJudgment, MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

import scala.collection.mutable

class ProgressGenerator(val problem: Problem, function: FunctionDef) extends StrategyHelpers {
  import Query._

  implicit private val enquirer = problem.enquirer

  def generateBase(): Seq[Lemma] = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successVar :: arguments = Assignments.generateSimpleSingle(function.successfulOutType +: function.inTypes)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    Seq(new Lemma(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  def constructAllChoices[T](choices: Seq[Seq[T]]): Seq[Seq[T]] = choices match {
    case currentChoices :: remainingChoices =>
      val constructedRemainingChoices: Seq[Seq[T]] = constructAllChoices(remainingChoices)
      for(currentChoice <- currentChoices; remainingChoice <- constructedRemainingChoices)
          yield currentChoice +: remainingChoice
    case Nil => Seq(Seq())
  }

  def constrainConsequenceVariables(lemma: Lemma): Set[Lemma] = {
    val consequenceVariables: Seq[MetaVar] = lemma.consequences.flatMap(
      enquirer.getUniversallyQuantifiedVars(_)
    ).collect {
      case mv@MetaVar(_) => mv
    }
    // find all static predicates and transformers that talk about these variables
    val varConstraints = consequenceVariables.map(mv =>
      (enquirer.retrievePredicates(Set(mv.sortType)) ++ enquirer.retrieveTransformers(Set(mv.sortType)))
        .filter(_.isStatic).toSeq)
    val choices = constructAllChoices(varConstraints)
    var lemmas = new mutable.HashSet[Lemma]()
    for(choice <- choices) {
      var currentLemmas = Set(lemma)
      for((mv, fn) <- consequenceVariables zip choice) {
        val assignmentConstraint = fn.inTypes.map {
          case typ if typ == mv.sortType => Constraint.fixed(mv) // TODO: might be multiple
          case typ => Constraint.fresh(typ)
        }
        currentLemmas = currentLemmas.flatMap(lemma => refine(lemma, fn.successfulOutType match {
            case SortRef("Bool") => selectPredicate(lemma, fn, assignmentConstraint)
            case typ => selectSuccessfulApplication(lemma, fn, assignmentConstraint, Constraint.fresh(typ))
          }))
      }
      lemmas ++= currentLemmas
    }
    lemmas.toSet
  }

  def addEquations(lemma: Lemma): Set[Lemma] = {
    val boundTypes = lemma.boundTypes
    boundTypes.flatMap(boundType => {
      val bindings = lemma.bindingsOfType(boundType)
      val possibleEquations = bindings.subsets.filter(_.size >= 2)
      possibleEquations.map(equalVars => {
        var refinedLemma = lemma
        for (List(left, right) <- equalVars.toList.sliding(2)) {
          val refinement = Refinement.Equation(FunctionMeta(left), FunctionMeta(right))
          refinedLemma = refinement.refine(problem, refinedLemma).getOrElse(refinedLemma)
        }
        refinedLemma
      })
    })
  }
}
