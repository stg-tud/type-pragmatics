package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.{GraphConstructor, PredicatePreservationConstructor, ProgressConstructor, RelationalPreservationConstructor}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenerator, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

/** Abstract class implementing the Clever and CleverHints algorithms for lemma generation.
  * The class is parametric in:
  *  - the oracle consultation, lemma selection and postprocessing steps
  *  - the processing of hints. More specifically, this class already
  *    supports the application of hints, but does not automatically do so:
  *    In order to apply hints, a subclass needs to implement the
  *    `makeHints` method. Hence, this class is used as a base for
  *    both Clever and CleverHints.
  */
abstract class AbstractCleverLemmaGenerator(problem: Problem) extends LemmaGenerator {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer = problem.enquirer

  // override in subclasses
  def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline

  /** Retrieve the hints matching `tag` for the given function `fn`.
    * If this returns None, there is a hint that suppresses the generation of lemmas.
    */
  def makeHints(tag: Seq[String], fn: FunctionDef): Option[Hints]

  /** Check if a given preservable is a relation */
  def isRelation(fn: FunctionDef): Boolean = {
    fn.inTypes.length == 2 && fn.inTypes.head == fn.inTypes(1)
  }

  /** Return all preservables that take one or more arguments of sort `termType` */
  def getPreservablesInvolving(termType: SortRef): Set[FunctionDef] = {
    enquirer.retrievePreservables(Set(termType))
  }

  /** Return all preservable predicates that take one or more arguments of sort `termType`.
    * This explicitly excludes relations. */
  def getPredicatesInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filterNot(isRelation)
  }

  /** Return all preservable relations that take one or more arguments of sort `termType` */
  def getRelationsInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filter(isRelation)
  }

  /** Select all functions for which preservation lemmas should be generated */
  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn =>
      fn.outType.name != "Bool" && fn.inTypes.contains(fn.successfulOutType)
    )
  }

  /** Select all functions for which progress lemmas should be generated */
  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.outType))
  }

  /** Generate preservation lemmas for the given dynamic function. */
  def generatePreservationLemmas(fn: FunctionDef): Seq[Lemma] = {
    val result = new mutable.HashSet[Lemma]()
    // find all preservable predicates (excluding relations) which take one or more arguments
    // of the successful return type of fn
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) {
      if(predicate.inTypes.count(_ == fn.successfulOutType) == 1) {
        result ++= tryInvokePipeline(makePredicatePreservationGraphConstructor(fn, predicate))
      }
    }
    // find all relations which take one or more arguments of the successful return type of fn
    for (relation <- getRelationsInvolving(fn.successfulOutType)) {
      val matchingIndices = fn.inTypes.zipWithIndex.collect {
        case (inType, idx) if inType == fn.successfulOutType => idx
      }
      for(idx <- matchingIndices) {
        result ++= tryInvokePipeline(makeRelationalPreservationGraphConstructor(fn, relation, idx))
      }
    }
    result.toSeq
  }

  /** Given a graph constructor, invoke the lemma generation pipeline and return the generated lemmas.
    * If `constructor` is None, return an empty sequence. */
  def tryInvokePipeline(constructor: Option[GraphConstructor]): Seq[Lemma] = {
    constructor.map(makePipeline(_).invokePipeline()).getOrElse(Seq())
  }

  override def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }

  /** Generate progress lemmas for the given dynamic function */
  def generateProgressLemmas(fn: FunctionDef): Seq[Lemma] = {
    tryInvokePipeline(makeProgressGraphConstructor(fn))
  }

  override def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    progressFunctions.map(fn => fn -> generateProgressLemmas(fn)).toMap
  }

  /** Retrieve the hints generated for the progress lemmas for `fn`, and construct a
    * corresponding GraphConstructor. If any matching hint suppresses lemma generator,
    * return None.
    */
  def makeProgressGraphConstructor(fn: FunctionDef): Option[GraphConstructor] = {
    makeHints(Seq("Progress"), fn).map(hints => new ProgressConstructor(problem, fn, hints))
  }

  /** Retrieve the hints generated for the predicate preservation lemmas for `fn` and `predicate`,
    * and construct a corresponding GraphConstructor. If any matching hint suppresses lemma generator,
    * return None.
    */
  def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): Option[GraphConstructor] = {
    makeHints(Seq("Preservation", predicate.signature.name), fn).map(hints =>
      new PredicatePreservationConstructor(problem, fn, predicate, hints))
  }

  /** Retrieve the hints generated for the relational preservation lemmas for `fn`, `predicate` and `argIndex`,
    * and construct a corresponding GraphConstructor. If any matching hint suppresses lemma generator,
    * return None.
    */
  def makeRelationalPreservationGraphConstructor(fn: FunctionDef,
                                                 relation: FunctionDef, argIndex: Int): Option[GraphConstructor] = {
    makeHints(Seq("Preservation", relation.signature.name, argIndex.toString), fn).map(hints =>
      new RelationalPreservationConstructor(problem, fn, relation, argIndex, hints))
  }
}
