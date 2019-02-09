package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, NotJudgment, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables

import scala.collection.mutable

class PreservationGenerator(val problem: Problem, function: FunctionDef, predicate: FunctionDef) extends StrategyHelpers {
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

  def generateBase(): Lemma = {
    // --------------------
    // [predicate]([], ...)
    val outType = function.successfulOutType
    // [producer]([], ...) =  []
    // producer arguments can be fresh or bound with matching types
    val outVar :: inVars = Assignments.generateSimpleSingle(outType +: function.inTypes)
    // the success variable can be any of the arguments of ``predicate``, with matching types
    val predicateArgs = generatePredicateArguments(outVar)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we find all inVars with matching type
    val matchingInVars = inVars.filter(_.sortType == outType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(inVars), outVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    for(inVar <- matchingInVars) {
      val constraints = predicate.inTypes.map(inType =>
        if(inType == inVar.sortType)
          Constraint.fixed(inVar)
        else
          Constraint.fresh(inType)
      )
      val assignment = Assignments.generate(constraints, lemma).head
      val refinement = Refinement.Predicate(predicate, Assignments.wrapMetaVars(assignment))
      lemma = refinement.refine(problem, lemma).getOrElse(lemma)
    }
    lemma
  }

  def restrictableVariables(lemma: Lemma): Set[MetaVar] = lemma.boundVariables.filterNot(_.sortType == termType)
  def preVariables(lemma: Lemma): Set[MetaVar] = lemma.boundVariables -- postVariables(lemma)
  def postVariables(lemma: Lemma): Set[MetaVar] = FreeVariables.freeVariables(lemma.consequences)

  def generateEquations(lemma: Lemma): Set[Refinement] = {
    val restrictable = restrictableVariables(lemma)
    val partitioned = restrictable.groupBy(_.sortType)
    var restrictions = new mutable.ListBuffer[Refinement]()
    for((typ, metaVars) <- partitioned) {
      if(metaVars.size > 1) {
        val equals = metaVars.subsets.filter(_.size == 2)
        for(equal <- equals) {
          val a = equal.head
          val b = equal.tail.head
          if(equal.exists(preVariables(lemma) contains _) && equal.exists(postVariables(lemma) contains _)) {
            restrictions += Refinement.Equation(a, FunctionMeta(b))
          }
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

  def generateApplications(lemma: Lemma): Set[Refinement] = {
    val sideArguments = problem.enquirer.getSideArgumentsTypes(function)
    val staticFunctions = problem.enquirer.staticFunctions.filter(_.signature.in.intersect(sideArguments).nonEmpty)
    staticFunctions.flatMap(staticFn =>
      if(!containsApplicationOf(lemma, staticFn)) {
        if (staticFn.signature.out.name == "Bool") {
          selectPredicate(lemma, staticFn)
        } else {
          var refinements = selectSuccessfulApplication(lemma, staticFn, Constraint.preferBound(staticFn.inTypes), Constraint.preferBound(staticFn.successfulOutType))
          // do not want refinements which pass the same argument twice
          refinements = refinements.filterNot(r => r.arguments.toSet.size != r.arguments.size)
          // do not want refinements which assume no change
          refinements = refinements.filterNot(r => r.arguments.contains(FunctionMeta(r.result)))
          // do not want refinements whose in arguments contain post variables
          val postVars: Set[FunctionExpMeta] = postVariables(lemma).map(FunctionMeta(_))
          refinements = refinements.filterNot(r => r.arguments.exists(arg => postVars.contains(arg)))
          refinements
        }
      } else
        Set()
    )
  }

  def generateRestrictions(lemma: Lemma): Set[Refinement] = {
    val restrictable = restrictableVariables(lemma)
    generateEquations(lemma) ++ generateApplications(lemma)
  }

  def negative(lemma: Lemma): Lemma = {
    val consequence = NotJudgment(lemma.consequences.head)
    new Lemma(lemma.name + "_NEGATIVE", lemma.premises, Seq(consequence))
  }

  def testLemmaAndNegative(lemma: Lemma): (Oracle.Answer, Oracle.Answer)  = {
    val neg = negative(lemma)
    (Oracle.invoke(problem, Set(lemma)), Oracle.invoke(problem, Set(neg)))
  }

  def askOracle(lemma: Lemma): Unit = {
    println(lemma)
    val answer = testLemmaAndNegative(lemma) match {
      case (Oracle.Inconclusive(), Oracle.Inconclusive()) => "too specific"
      case (Oracle.Inconclusive(), Oracle.ProvablyFalse(_)) => "good lemma yay"
      case (Oracle.ProvablyFalse(_), Oracle.Inconclusive()) => "not sure though"
      case (Oracle.ProvablyFalse(_), Oracle.ProvablyFalse(_)) => "NOT REALLY"
      case (a, b) => sys.error(s"oracle said something weird: $a, $b")
    }
    println(answer)
  }

  def updateStatus(node: RefinementNode): Unit = {
    println(node.lemma)
    val answer = testLemmaAndNegative(node.lemma)
    println(answer match {
      case (Oracle.Inconclusive(), Oracle.Inconclusive()) => "too specific"
      case (Oracle.Inconclusive(), Oracle.ProvablyFalse(_)) => "good lemma yay"
      case (Oracle.ProvablyFalse(_), Oracle.Inconclusive()) => "not sure though"
      case (Oracle.ProvablyFalse(_), Oracle.ProvablyFalse(_)) => "NOT REALLY"
      case (a, b) => sys.error(s"oracle said something weird: $a, $b")
    })
    val oracleStatus = answer match {
      case (Oracle.Inconclusive(), _) => Inconclusive()
      case (Oracle.ProvablyFalse(_), Oracle.ProvablyFalse(_)) => Disproved()
      case _ => sys.error("unknown oracle status")
    }
    node.provabilityStatus = oracleStatus
  }

  def generate(): Seq[Lemma] = {
    /*val tree = new RefinementGraph(generateBase())
    tree.root.refinementStatus = ShouldRefine()
    var changedAnything = true
    while(changedAnything) {
      changedAnything = false
      val unknownNodes = tree.collectNodes(Unknown()).toSeq
      println(s"${unknownNodes.size} unknown nodes")
      for ((node, idx) <- unknownNodes.zipWithIndex) {
        println(s"=== ${idx+1}/${unknownNodes.size} ===")
        updateStatus(node)
        changedAnything = true
      }
      val incompleteNodes = tree.collectNodes(ShouldRefine())
      println(s"${incompleteNodes.size} incomplete nodes")
      for(node <- incompleteNodes) {
        val restrictions = generateRestrictions(node.lemma)
        println("====")
        for (restriction <- restrictions) {
          println("--->" + restriction)
          /*val neg = negative(restriction.refineNeg(problem, node.lemma).getOrElse(node.lemma))
          println("NEGATIVE: " + Oracle.invoke(problem, Set(neg)))*/
          node.refine(problem, restriction)
        }
        node.refinementStatus = Refined()
        changedAnything = true
      }
      println("and next!")
    }
    tree.visualize(new File(s"pres-${function.signature.name}.png") )
    /*val refinement = Refinement.Equation(MetaVar("tt"), FunctionMeta(MetaVar("tt2")))
    val foo = tree.root.refine(problem, refinement)
    askOracle(foo.lemma)
    val refinement2 = Refinement.Equation(MetaVar("tt"), FunctionMeta(MetaVar("tt1")))
    val leaf = foo.refine(problem, refinement2)
    askOracle(leaf.lemma)
    //tree.prune(problem, tree.nodes.toSeq)
    tree.visualizeRT(new File("preservation.png"), "png")
    println(leaf.lemma)*/
    tree.collectNodes(Inconclusive()).map(_.lemma).toSeq*/
    Seq()
  }
}
