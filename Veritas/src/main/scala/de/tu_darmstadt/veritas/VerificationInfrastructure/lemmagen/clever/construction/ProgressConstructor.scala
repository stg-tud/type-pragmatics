package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Assignments
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

class ProgressConstructor(val problem: Problem,
                          function: FunctionDef,
                          val hints: Hints) extends LemmaGraphConstructor {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  private val functionArguments = Assignments.generateSimpleSingle(function.inTypes)
  private val baseLemma = {
    val (failConstructor, _) = enquirer.retrieveFailableConstructors(function.outType)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(functionArguments))
    val failExp = FunctionExpApp(failConstructor.name, Seq())
    val inequality = enquirer.makeInequation(invocationExp, failExp).asInstanceOf[FunctionExpJudgment]
    new Lemma(s"${function.name}Progress", Seq(), Seq(inequality))
  }

  override def generateBase(): AnnotatedLemma = AnnotatedLemma(baseLemma, Set(), Set())
  override def invocationArguments: Seq[MetaVar] = functionArguments
  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = node.lemma.boundVariables
}
