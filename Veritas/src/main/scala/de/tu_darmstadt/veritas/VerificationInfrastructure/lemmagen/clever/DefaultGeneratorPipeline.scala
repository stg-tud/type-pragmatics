package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

class DefaultGeneratorPipeline(val problem: Problem) extends LemmaGeneratorPipeline {
  def makeHints(tag: String, fn: FunctionDef): Hints = Hints.fromDSK(problem, fn, tag)

  override def makeProgressGraphConstructor(fn: FunctionDef): GraphConstructor = {
    new ProgressConstructor(problem, fn,
      makeHints(s"progress/${fn.signature.name}", fn))
  }

  override def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): GraphConstructor = {
    new PredicatePreservationConstructor(problem, fn, predicate,
      makeHints(s"preservation/predicate/${fn.signature.name}", fn))
  }

  override def makeRelationalPreservationGraphConstructor(fn: FunctionDef,
                                                          relation: FunctionDef, termIndex: Int): GraphConstructor = {
    new RelationalPreservationConstructor(problem, fn, relation, termIndex,
      makeHints(s"preservation/relational/${fn.signature.name}/$termIndex", fn))
  }

  override def makeOracleConsultation(): OracleConsultation = new VampireOracleConsultation(problem)

  override def makeExtractionHeuristic(): ExtractionHeuristic = new DefaultHeuristic()

  override def makePostProcessor(): Postprocessor = new DefaultPostprocessor(problem)
}
