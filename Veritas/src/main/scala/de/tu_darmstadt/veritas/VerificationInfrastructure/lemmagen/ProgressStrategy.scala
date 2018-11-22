package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

import scala.collection.mutable

class ProgressStrategy(problem: Problem, function: FunctionDef) extends RefinementStrategy {
  implicit private val enquirer = problem.enquirer
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  override def generateBase(): Seq[Lemma] = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successName = FreshVariables.freshName(Set(), prefix = "success") // TODO: this is only to avoid name clashes
    val successVar = MetaVar(successName)
    successVar.typ = Some(TypeInference.Sort(successConstructor.in.head.name))
    val arguments = FreshVariables.freshMetaVars(Set(), function.inTypes)
    val invocationExp = FunctionExpApp(function.name, arguments.map {
      case (metaVar, typ) => FunctionMeta(metaVar)
    })
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    Seq(new Lemma(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(_.isFailable)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    val failableTransformers = transformers.filter(_.isFailable)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    predicates.foreach(fn => {
      val assignments = generateAssignments(lemma, fn.inTypes)
      refinements ++= assignments.map(assignment => Refinement.Predicate(fn, wrapMetaVars(assignment)))
    })
    (failableProducers ++ failableTransformers).collect({
      case fn if fn.isStatic => {
        val assignments = generateAssignments(lemma, fn.inTypes)
        refinements ++= assignments.map(assignment => {
          val successVar = FreshVariables.freshMetaVar(lemma.freeVariables ++ assignment.toSet, fn.successfulOutType)
          Refinement.SuccessPredicate(fn, wrapMetaVars(assignment), successVar)
        })
      }
    })
    refinements
  }

  def constructAllChoices[T](choices: Seq[Seq[T]]): Seq[Seq[T]] = choices match {
    case currentChoices :: remainingChoices =>
      val constructedRemainingChoices: Seq[Seq[T]] = constructAllChoices(remainingChoices)
      for(currentChoice <- currentChoices; remainingChoice <- constructedRemainingChoices)
        yield currentChoice +: remainingChoice
    case Nil => Seq(Seq())
  }

  def generateAssignments(lemma: Lemma, types: Seq[SortRef],
                          prefix: Seq[MetaVar] = Seq()): Seq[Seq[MetaVar]] = types match {
    case Nil => Seq(prefix)
    case head :: tail =>
      // use bound if there are variables of that type. TODO
      val choices = lemma.bindingsOfType(head).toSeq
      if(choices.isEmpty) {
        // bind a new variable
        val mv = FreshVariables.freshMetaVar(lemma.freeVariables ++ prefix.toSet, head)
        generateAssignments(lemma, tail, prefix :+ mv)
      } else {
        choices.flatMap(mv => generateAssignments(lemma, tail, prefix :+ mv))
      }
  }

  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))
}
