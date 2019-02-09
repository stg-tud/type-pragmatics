package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable

class ProgressGenerator(val problem: Problem, function: FunctionDef) extends StrategyHelpers {
  import Query._

  implicit private val enquirer = problem.enquirer

  /*
  def generateBase(): Lemma = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val assignments = Assignments.generateSimpleSingle(function.inTypes :+ function.successfulOutType)
    val arguments = assignments.init
    val successVar = assignments.last
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    new Lemma(s"${function.name}Progress", Seq(), Seq(exists))
  }*/

  def generateBase(): Lemma = {
    val (failConstructor, _) = enquirer.retrieveFailableConstructors(function.outType)
    val arguments = Assignments.generateSimpleSingle(function.inTypes)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
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
          val a = equal.head
          val b = equal.tail.head
          if(equal.exists(node.preVariables contains _) && equal.exists(node.postVariables contains _)) {
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

  def generateRestrictions(node: RefinementNode): Set[Refinement] = {
    generateEquations(node) ++ generateApplications(node)
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

  def updateStatus(node: RefinementNode): (OracleStatus, OracleStatus) = {
    println(node.lemma)
    val answer = testLemmaAndNegative(node.lemma)
    println(answer match {
      case (Oracle.Inconclusive(), Oracle.Inconclusive()) => "too specific"
      case (Oracle.Inconclusive(), Oracle.ProvablyFalse(_)) => "good lemma yay"
      case (Oracle.ProvablyFalse(_), Oracle.Inconclusive()) => "TODO not sure though"
      case (Oracle.ProvablyFalse(_), Oracle.ProvablyFalse(_)) => "NOT REALLY"
      case (a, b) => sys.error(s"oracle said something weird: $a, $b")
    })
    def translate(x: Oracle.Answer): OracleStatus = x match {
      case Oracle.Inconclusive() => Inconclusive()
      case Oracle.ProvablyFalse(_) => Incorrect()
    }
    (translate(answer._1), translate(answer._2))
  }

  def updateTree(tree: RefinementTree): Unit = {
    var fringe = new mutable.Queue[RefinementNode]()
    fringe ++= tree.leaves
    while(fringe.nonEmpty) {
      var node = fringe.dequeue()
      fringe ++= node.parents
      while(node.oracleStatus != Unknown() && fringe.nonEmpty) {
        node = fringe.dequeue()
        fringe ++= node.parents
      }
      println(s"${fringe.size} elements")
      if(node.oracleStatus == Unknown()) {
        val (status, statusInv) = updateStatus(node)
        node.invertedStatus = statusInv
        if (status == Incorrect()) {
          node.setStatusRecursively(status)
        } else {
          node.oracleStatus = status
        }
      }
    }
  }

  def generate(): Seq[Lemma] = {
    val tree = new RefinementTree(generateBase())
    var changedAnything = true
    while(changedAnything) {
      changedAnything = false
      /*val unknownNodes = tree.collectNodes(Unknown()).toSeq
      println(s"${unknownNodes.size} unknown nodes")
      for ((node, idx) <- unknownNodes.zipWithIndex) {
        println(s"=== ${idx+1}/${unknownNodes.size} ===")
        updateStatus(node)
        changedAnything = true
      }*/
      val incompleteNodes = tree.collectNodes(ShouldRefine())
      println(s"${incompleteNodes.size} incomplete nodes")
      for(node <- incompleteNodes) {
        val restrictions = generateRestrictions(node)
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
    tree.visualizeRT(new File(s"prog-${function.signature.name}-before.png") )
    updateTree(tree)
    val heuristic = new RankingHeuristic(tree)
    val incLemmas = heuristic.extract().map(_.lemma)
    tree.visualizeRT(new File(s"prog-${function.signature.name}-after.png") )
    incLemmas.map(lemma =>
      lemma.consequences.head match {
        case FunctionExpJudgment(FunctionExpNeq(l, r)) =>
          val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
          val assignments = Assignments.generate(Seq(Constraint.fresh(function.successfulOutType)), lemma.boundVariables)
          val successVar = assignments.head.head
          val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
          val equality = enquirer.makeEquation(l, successExp).asInstanceOf[FunctionExpJudgment]
          val exists = ExistsJudgment(Seq(successVar), Seq(equality))
          new Lemma(lemma.name, lemma.premises, Seq(exists), lemma.refinements)
        case _ => sys.error("TODO")
      })
  }

}
