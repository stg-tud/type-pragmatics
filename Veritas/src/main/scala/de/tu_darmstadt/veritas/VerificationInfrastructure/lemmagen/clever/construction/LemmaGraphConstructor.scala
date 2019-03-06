package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Constraint, StrategyHelpers}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenSpecEnquirer, Refinement}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpMeta, FunctionMeta}

trait LemmaGraphConstructor extends GraphConstructor with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def generateBase(): AnnotatedLemma
  def hints: Hints
  def invocationArguments: Seq[MetaVar]
  def restrictableVariables(node: RefinementNode): Set[MetaVar]

  def generateEquations(node: RefinementNode): Set[Refinement] = {
    val partitioned = restrictableVariables(node).groupBy(_.sortType)
    partitioned.flatMap {
      case (_, metaVars) =>
        for(equal <- metaVars.subsets.filter(_.size == 2) if !equal.subsetOf(invocationArguments.toSet))
          yield Refinement.Equation(equal.head, equal.tail.head)
    }.toSet
  }

  def possibleRefinements(lemma: Lemma, function: FunctionDef): Seq[Refinement] = {
    if(function.outType.name == "Bool")
      selectPredicate(lemma, function)
    else
      selectSuccessfulApplication(lemma, function,
        Constraint.preferBound(function.inTypes),
        Constraint.preferBound(function.successfulOutType))
  }

  def generateApplications(node: RefinementNode): Set[Refinement] = {
    val invocationTypes = invocationArguments.map(_.sortType)
    val notConstrainedYet: Set[FunctionExpMeta] = (node.lemma.boundVariables -- node.constrainedVariables).map(FunctionMeta(_))
    val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
    val relevantFunctions = problem.enquirer.staticFunctions.filter(_.inTypes.intersect(invocationTypes).nonEmpty)
    relevantFunctions.flatMap { staticFn =>
      possibleRefinements(node.lemma, staticFn).filter { refinement =>
        val arguments = refinement match {
          case Refinement.Predicate(_, a) => a.toSet
          case Refinement.SuccessfulApplication(_, a, _) => a.toSet
        }
        arguments.intersect(notConstrainedYet).nonEmpty && arguments.intersect(postVars).isEmpty
      }
     }
  }

  override def expand(node: RefinementNode): Set[Refinement] = {
    generateEquations(node) ++ generateApplications(node)
  }

  override def constructRoot(): AnnotatedLemma = {
    val base = generateBase()
    hints.apply(base)
  }
}
