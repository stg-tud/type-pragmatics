package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Assignments
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

import scala.collection.mutable

class ProgressStrategy(override val problem: Problem, function: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import Query._

  implicit private val enquirer = problem.enquirer

  override def generateBase(): Seq[Lemma] = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successVar :: arguments = Assignments.generateSimpleSingle(function.successfulOutType +: function.inTypes)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    Seq(new Lemma(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = enquirer.retrievePredicates(lemma.boundTypes)
    val producers = enquirer.retrieveProducers(lemma.boundTypes)
    val transformers = enquirer.retrieveTransformers(lemma.boundTypes)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates if predicate.isStatic)
      refinements ++= selectPredicate(lemma, predicate)
    /*for(fn <- producers if fn.isStatic && fn.isFailable)
      refinements ++= selectSuccessPredicate(lemma, fn)*/ // TODO: Apparently we do not need this
    for(fn <- transformers if fn.isStatic && fn.isFailable)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
