package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints.AdditionalPremises
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

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

  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn =>
      fn.outType.name != "Bool" && fn.inTypes.contains(fn.successfulOutType)
    )
  }

  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.outType))
  }

  def generatePreservationLemmas(fn: FunctionDef): Set[Lemma] = {
    val result = new mutable.HashSet[Lemma]()
    val hint = AdditionalPremises.fromDSK(problem, fn)
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) {
      println(s"${fn.signature.name} / ${predicate.signature.name}")
      val generator = new PreservationGenerator(problem, fn, predicate, Seq(hint))
      result ++= generator.generate()
    }
    result.toSet
  }
}
