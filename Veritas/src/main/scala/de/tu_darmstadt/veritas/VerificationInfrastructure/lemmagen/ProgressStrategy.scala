package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference

import scala.collection.mutable

class ProgressStrategy(override val problem: Problem, function: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._
  import Assignments._

  implicit private val enquirer = problem.enquirer

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
    for(predicate <- predicates)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- failableProducers if fn.isStatic)
      refinements ++= selectSuccessPredicate(lemma, fn)
    for(fn <- failableTransformers if fn.isStatic)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
