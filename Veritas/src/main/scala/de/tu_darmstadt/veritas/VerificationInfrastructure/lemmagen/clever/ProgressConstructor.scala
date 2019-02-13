package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable

class ProgressConstructor(val problem: Problem, function: FunctionDef, hints: Option[Hints])
  extends GraphConstructor with StrategyHelpers {
  import Query._

  require(hints == None)

  implicit private val enquirer = problem.enquirer

  private val functionArguments = Assignments.generateSimpleSingle(function.inTypes)
  private val baseLemma = {
    val (failConstructor, _) = enquirer.retrieveFailableConstructors(function.outType)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(functionArguments))
    val failExp = FunctionExpApp(failConstructor.name, Seq())
    val inequality = enquirer.makeInequation(invocationExp, failExp).asInstanceOf[FunctionExpJudgment]
    new Lemma(s"${function.name}Progress", Seq(), Seq(inequality))
  }

  def generateEquations(node: RefinementNode): Set[Refinement] = {
    val restrictable = node.lemma.boundVariables
    val partitioned = restrictable.groupBy(_.sortType)
    var restrictions = new mutable.ListBuffer[Refinement]()
    for((typ, metaVars) <- partitioned) {
      if(metaVars.size > 1) {
        val equals = metaVars.subsets.filter(_.size == 2)
        for(equal <- equals) {
          if(!equal.subsetOf(functionArguments.toSet))
            restrictions += Refinement.Equation(equal.head, FunctionMeta(equal.tail.head))
        }
      }
    }
    restrictions.toSet
  }

  def generateApplications(node: RefinementNode): Set[Refinement] = {
    val sideArguments = function.inTypes
    val notConstrainedYet: Set[FunctionExpMeta] = (node.lemma.boundVariables -- node.constrainedVariables).map(FunctionMeta(_))
    val staticFunctions = problem.enquirer.staticFunctions.filter(_.signature.in.intersect(sideArguments).nonEmpty)
    staticFunctions.flatMap(staticFn =>
        if (staticFn.signature.out.name == "Bool") {
          var refinements = selectPredicate(node.lemma, staticFn)
          refinements = refinements.filter(r => r.arguments.toSet.intersect(notConstrainedYet).nonEmpty)
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        } else {
          var refinements = selectSuccessfulApplication(node.lemma, staticFn, Constraint.preferBound(staticFn.inTypes), Constraint.preferBound(staticFn.successfulOutType))
          refinements = refinements.filter(r => r.arguments.toSet.intersect(notConstrainedYet).nonEmpty)
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements which assume no change
          refinements = refinements.filterNot(r => r.arguments.contains(FunctionMeta(r.result)))
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        }
    )
  }

  override def constructRoot(): AnnotatedLemma = {
    // TODO: Hints
    AnnotatedLemma(baseLemma, Set(), Set())
  }

  override def expand(node: RefinementNode): Set[Refinement] = {
    generateEquations(node) ++ generateApplications(node)
  }
}
