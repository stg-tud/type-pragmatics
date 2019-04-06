package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenerator, Problem}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

/** The NaiveLemmaGenerator implements the Naive algorithm with a fixed maximum number of premises. */
class NaiveLemmaGenerator(problem: Problem, maxPremises: Int = 4) extends LemmaGenerator {
  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filterNot(fn => fn.signature.out.name == "Bool")
  }

  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.signature.out))
  }

  /** Use a LimitedDepthLemmaRefinery to generate lemmas according to `strategy`, and return them. */
  def generateWithStrategy(strategy: RefinementStrategy): Seq[Lemma] = {
    val generator = new LimitedDepthLemmaRefinery(problem, strategy, maxPremises)
    generator.generate()
  }

  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map { fn =>
      (fn, generateWithStrategy(new PreservationStrategy(problem, fn)))
    }.toMap
  }

  def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    progressFunctions.toSeq.map { fn =>
      (fn, generateWithStrategy(new ProgressStrategy(problem, fn)))
    }.toMap
  }
}
