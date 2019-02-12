package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints.Hint
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables

import scala.collection.mutable

class ProgressConstructor(val problem: Problem, function: FunctionDef, hints: Seq[Hint])
  extends GraphConstructor[RefinementGraph] with StrategyHelpers {
  import Query._

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

  def containsApplicationOf(lemma: Lemma, fn: FunctionDef): Boolean = {
    lemma.refinements.collect {
      case SuccessfulApplication(func, _, _) if fn == func => fn
      case Predicate(func, _) if fn == func => fn
    }.nonEmpty
  }

  def generateApplications(node: RefinementNode): Set[Refinement] = {
    val sideArguments = function.inTypes
    val staticFunctions = problem.enquirer.staticFunctions.filter(_.signature.in.intersect(sideArguments).nonEmpty)
    staticFunctions.flatMap(staticFn =>
      if(!containsApplicationOf(node.lemma, staticFn)) {
        if (staticFn.signature.out.name == "Bool") {
          var refinements = selectPredicate(node.lemma, staticFn)
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        } else {
          var refinements = selectSuccessfulApplication(node.lemma, staticFn, Constraint.preferBound(staticFn.inTypes), Constraint.preferBound(staticFn.successfulOutType))
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements which assume no change
          refinements = refinements.filterNot(r => r.arguments.contains(FunctionMeta(r.result)))
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        }
      } else
        Set()
    )
  }

  def generateRefinements(node: RefinementNode): Set[Refinement] = {
    generateEquations(node) ++ generateApplications(node)
  }

  def construct(): RefinementGraph = {
    val graph = new RefinementGraph(baseLemma, Set())
    while(graph.openNodes.nonEmpty) {
      for(node <- graph.openNodes) {
        val restrictions = generateRefinements(node)
        for (restriction <- restrictions) {
          node.refine(problem, restriction)
        }
        node.open = false
      }
    }
    graph
  }
}
