package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LimitedDepthLemmaGenerator, Problem, RefinementStrategy}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

class NaiveLemmaGenerator(problem: Problem, maxPremises: Int = 4) {
  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filterNot(fn => fn.signature.out.name == "Bool")
  }

  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.signature.out))
  }

  def generateWithStrategy(strategy: RefinementStrategy): Set[Lemma] = {
    val generator = new LimitedDepthLemmaGenerator(problem, strategy, maxPremises)
    generator.generate().toSet
  }

  def generate(): Map[FunctionDef, Set[Lemma]] = {
    val generatedLemmas = new mutable.HashMap[FunctionDef, Set[Lemma]]()
    preservationFunctions.foreach { fn =>
      if(!generatedLemmas.contains(fn))
        generatedLemmas(fn) = Set()
      println(s"generate preservation lemmas for ${fn.signature.name}")
      generatedLemmas(fn) ++= generateWithStrategy(new PreservationStrategy(problem, fn))
    }
    progressFunctions.foreach { fn =>
      if(!generatedLemmas.contains(fn))
        generatedLemmas(fn) = Set()
      println(s"generate progress lemmas for ${fn.signature.name}")
      generatedLemmas(fn) ++= generateWithStrategy(new ProgressStrategy(problem, fn))
    }
    generatedLemmas.toMap
  }
}
