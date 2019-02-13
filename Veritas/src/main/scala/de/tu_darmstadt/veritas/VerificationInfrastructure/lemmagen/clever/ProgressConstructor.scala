package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable

class ProgressConstructor(val problem: Problem,
                          function: FunctionDef,
                          hints: Option[Hints]) extends LemmaGraphConstructor {
  import Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  require(hints.isEmpty)

  private val functionArguments = Assignments.generateSimpleSingle(function.inTypes)
  private val baseLemma = {
    val (failConstructor, _) = enquirer.retrieveFailableConstructors(function.outType)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(functionArguments))
    val failExp = FunctionExpApp(failConstructor.name, Seq())
    val inequality = enquirer.makeInequation(invocationExp, failExp).asInstanceOf[FunctionExpJudgment]
    new Lemma(s"${function.name}Progress", Seq(), Seq(inequality))
  }

  override def invocationArguments: Seq[MetaVar] = functionArguments
  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = node.lemma.boundVariables

  override def constructRoot(): AnnotatedLemma = {
    // TODO: Hints
    AnnotatedLemma(baseLemma, Set(), Set())
  }
}
