package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, StrategyHelpers}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference

import scala.collection.mutable

/** Refinement Strategy for generating progress lemmas for a specific function. */
class ProgressStrategy(override val problem: Problem, function: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._

  implicit private val enquirer = problem.enquirer

  override def generateBase(): Seq[Lemma] = {
    // retrieve the success constructor for function's failable return sort
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    // use "result" as name to avoid name clashes with variables that are introduced later
    val successVar = MetaVar("result")
    successVar.typ = Some(TypeInference.Sort(function.successfulOutType.name))
    // build existential formula for consequence
    val arguments = Assignments.generateSimpleSingle(function.inTypes)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    Seq(new Lemma(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = enquirer.retrievePredicates(lemma.boundTypes)
    val transformers = enquirer.retrieveTransformers(lemma.boundTypes)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates if predicate.isStatic)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- transformers if fn.isStatic && fn.isFailable)
      refinements ++= selectSuccessfulApplication(lemma, fn)
    refinements
  }
}
