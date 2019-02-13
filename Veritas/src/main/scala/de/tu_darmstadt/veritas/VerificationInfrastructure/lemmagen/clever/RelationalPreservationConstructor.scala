package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, NotJudgment, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable

class RelationalPreservationConstructor(val problem: Problem,
                                       function: FunctionDef,
                                       predicate: FunctionDef,
                                       hints: Option[Hints]) extends GraphConstructor with StrategyHelpers {
  import Query._

  implicit private val enquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head // TODO
  }

  // --------------------
  // [predicate]([t_1], [t_2])
  // [producer]([], ...) =  []
  // producer arguments can be fresh or bound with matching types
  // the success variable can be any of the arguments of ``predicate``, with matching types
  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  private val matchingInVars = functionArgs.filter(_.sortType == termType)
  require(matchingInVars.size == 1)
  private val inVar = matchingInVars.head
  private val relationConstraints = Seq(Constraint.fixed(inVar), Constraint.fixed(resultVar))
  private val predicateArgs = Assignments.generate(relationConstraints).head

  def generateBase(): AnnotatedLemma = {
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we find all inVars with matching type
    //val matchingInVars = inVars.filter(_.sortType == outType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    AnnotatedLemma(lemma, predicateArgs.toSet, Set(resultVar))
  }

  def restrictableVariables(lemma: Lemma): Set[MetaVar] = lemma.boundVariables.filterNot(_.sortType == termType)

  def generateEquations(node: RefinementNode): Set[Refinement] = {
    val lemma = node.lemma
    val restrictable = restrictableVariables(lemma)
    val partitioned = restrictable.groupBy(_.sortType)
    var restrictions = new mutable.ListBuffer[Refinement]()
    for((typ, metaVars) <- partitioned) {
      if(metaVars.size > 1) {
        val equals = metaVars.subsets.filter(_.size == 2)
        for(equal <- equals) {
          if(!equal.subsetOf(functionArgs.toSet))
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
    val lemma = node.lemma
    val sideArguments = problem.enquirer.getSideArgumentsTypes(function)
    val staticFunctions = problem.enquirer.staticFunctions.filter(_.signature.in.intersect(sideArguments).nonEmpty)
    staticFunctions.flatMap(staticFn =>
      if (!containsApplicationOf(lemma, staticFn)) {
        if (staticFn.signature.out.name == "Bool") {
          selectPredicate(lemma, staticFn)
        } else {
          var refinements = selectSuccessfulApplication(lemma, staticFn, Constraint.preferBound(staticFn.inTypes), Constraint.preferBound(staticFn.successfulOutType))
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements which assume no change
          refinements = refinements.filterNot(r => r.arguments.contains(FunctionMeta(r.result)))
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = node.postVariables.map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        }
      } else {
        Set()
      }
    )
  }

  def generateRefinements(node: RefinementNode): Set[Refinement] = {
    generateEquations(node) ++ generateApplications(node)
  }

  def generateBaseWithHints(): AnnotatedLemma = {
    val base = generateBase()
    hints match {
      case None => base
      case Some(actualHints) => actualHints.apply(base)
    }
  }

  def construct(): RefinementGraph = {
    val root = new RefinementNode(generateBaseWithHints())
    val graph = new RefinementGraph(problem, root)
    while(graph.openNodes.nonEmpty) {
      for(node <- graph.openNodes) {
        val restrictions = generateRefinements(node)
        for (restriction <- restrictions) {
          graph.refine(node, restriction)
        }
        node.open = false
      }
    }
    graph
  }
}
