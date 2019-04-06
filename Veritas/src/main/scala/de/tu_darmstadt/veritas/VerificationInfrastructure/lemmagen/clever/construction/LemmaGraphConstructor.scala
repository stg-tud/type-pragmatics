package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Constraint, StrategyHelpers}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenSpecEnquirer, Refinement}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpMeta, FunctionMeta}

/** LemmaGraphConstructor extends GraphConstructor with some additional helper methods. */
trait LemmaGraphConstructor extends GraphConstructor with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  // base generation, to be implemented by subclasses
  def generateBase(): AnnotatedLemma
  // hints that should be applied to the root lemma
  def hints: Hints
  // to be implemented by subclasses
  def invocationArguments: Seq[MetaVar]
  def restrictableVariables(node: RefinementNode): Set[MetaVar]

  /** Generate a set of `Equation` refinements for `node` */
  def generateEquations(node: RefinementNode): Set[Refinement] = {
    val partitioned = restrictableVariables(node).groupBy(_.sortType)
    partitioned.flatMap {
      case (_, metaVars) =>
        for(equal <- metaVars.subsets.filter(_.size == 2) if !equal.subsetOf(invocationArguments.toSet))
          yield Refinement.Equation(equal.head, equal.tail.head)
    }.toSet
  }

  /** Generate a set of possible refinements for `lemma` and `function`. */
  def possibleRefinements(lemma: Lemma, function: FunctionDef): Seq[Refinement] = {
    if(function.outType.name == "Bool")
      selectPredicate(lemma, function)
    else
      selectSuccessfulApplication(lemma, function,
        Constraint.preferBound(function.inTypes),
        Constraint.preferBound(function.successfulOutType))
  }

  /** Generate a set of `SuccessfulApplication` and `Predicate` refinements for `node`. */
  def generateApplications(node: RefinementNode): Set[Refinement] = {
    val invocationTypes = invocationArguments.map(_.sortType)
    val notConstrainedYet: Set[FunctionExpMeta] = (node.lemma.boundVariables -- node.constrainedVariables).map(FunctionMeta(_))
    val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
    val relevantFunctions = problem.enquirer.staticFunctions.filter(_.inTypes.intersect(invocationTypes).nonEmpty)
    relevantFunctions.flatMap { staticFn =>
      possibleRefinements(node.lemma, staticFn).filter { refinement =>
        // filter the result of `possibleRefinements`: only retain refinements which mention
        // at least one non-constrained variable and which do not mention any post vars.
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

  /** Construct a root node by invoking `generateBase` and applying `hints`. */
  override def constructRoot(): AnnotatedLemma = {
    val base = generateBase()
    hints.apply(base)
  }
}
