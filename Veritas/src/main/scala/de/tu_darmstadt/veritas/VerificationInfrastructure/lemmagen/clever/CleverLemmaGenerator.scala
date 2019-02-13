package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.{ExistsJudgment, FunctionExpJudgment, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpNeq, FunctionMeta}

import scala.collection.mutable

class CleverLemmaGenerator(problem: Problem) {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Query._

  implicit private val enquirer = problem.enquirer

  def isRelation(fn: FunctionDef): Boolean = {
    fn.inTypes.length == 2 && fn.inTypes.head == fn.inTypes(1)
  }

  def getPreservablesInvolving(termType: SortRef): Set[FunctionDef] = {
    enquirer.retrievePredicates(Set(termType)) intersect problem.dsk.preservables
  }

  def getPredicatesInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filterNot(isRelation)
  }

  def getRelationsInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filter(isRelation)
  }

  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn =>
      fn.outType.name != "Bool" && fn.inTypes.contains(fn.successfulOutType)
    )
  }

  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.outType))
  }

  def generateWithConstructor(constructor: GraphConstructor[RefinementGraph],
                              directory: String): Seq[Lemma] = {
    val consultation = new VampireOracleConsultation(problem)
    val extractor = new RankingHeuristic()
    val pipeline = new VisualizingGenerationPipeline(
      constructor,
      consultation,
      extractor,
      new File(directory)
    )
    pipeline.generate()
  }

  def generatePreservationLemmas(fn: FunctionDef): Set[Lemma] = {
    val result = new mutable.HashSet[Lemma]()
    val hints = Hints.fromDSK(problem, fn)
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) {
      println(s"${fn.signature.name} / ${predicate.signature.name}")
      val constructor = new PredicatePreservationConstructor(problem, fn, predicate, Some(hints))
      result ++= generateWithConstructor(
        constructor,
        s"generated/preservation/${fn.signature.name}/${predicate.name}"
      )
    }
    if(fn.inTypes.count(_ == fn.successfulOutType) == 1) {
      for (relation <- getRelationsInvolving(fn.successfulOutType)) {
        println(s"${fn.signature.name} / ${relation.signature.name}")
        val constructor = new RelationalPreservationConstructor(problem, fn, relation, Some(hints))
        result ++= generateWithConstructor(
          constructor,
          s"generated/preservation/${fn.signature.name}/${relation.name}"
        )
      }
    }
    result.toSet
  }

  def generateProgressLemmas(fn: FunctionDef): Set[Lemma] = {
    //val hint = AdditionalPremises.fromDSK(problem, fn) TODO
    println(s"${fn.signature.name}")
    val constructor = new ProgressConstructor(problem, fn, None)
    val consultation = new VampireOracleConsultation(problem)
    val extractor = new RankingHeuristic()
    val pipeline = new VisualizingGenerationPipeline(
      constructor,
      consultation,
      extractor,
      new File(s"generated/progress/${fn.signature.name}/")
    )
    pipeline.generate().map(lemma =>
      lemma.consequences.head match {
        case FunctionExpJudgment(FunctionExpNeq(l, r)) =>
          val (_, successConstructor) = enquirer.retrieveFailableConstructors(fn.outType)
          val assignments = Assignments.generate(Seq(Constraint.fresh(fn.successfulOutType)), lemma.boundVariables)
          val successVar = assignments.head.head
          val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
          val equality = enquirer.makeEquation(l, successExp).asInstanceOf[FunctionExpJudgment]
          val exists = ExistsJudgment(Seq(successVar), Seq(equality))
          new Lemma(lemma.name, lemma.premises, Seq(exists), lemma.refinements)
        case _ => sys.error("TODO")
      }).toSet // TODO: this needs to go somewhere else
  }

  def generatePreservationLemmas(): Map[FunctionDef, Set[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }
}
