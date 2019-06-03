package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Equation, Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem, Refinement}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.util.FreeVariables

/** An annotated lemma contains a lemma, the set of constrained variables and the set of post variables. */
case class AnnotatedLemma(lemma: Lemma,
                          constrainedVariables: Set[MetaVar],
                          postVariables: Set[MetaVar]) {
  /** Check if the annotated lemma is \approx-equivalent to `right`.
    * This also takes the set of constrained and post variables into account.
    */
  def equivalent(right: AnnotatedLemma): Boolean = right match {
    case AnnotatedLemma(rightLemma, rightConstrained, rightPost) =>
      LemmaEquivalence.findHarmonizingRenaming(lemma, rightLemma) match {
        case None => false
        case Some(renaming) =>
          // the two annotated lemmas are only equivalent if the
          // constrained and post variables match (modulo the renaming needed
          // to transform `right` to `this`)
          val rightConstrainedRenamed = rightConstrained.map(renaming)
          val rightPostRenamed = rightPost.map(renaming)
          (LemmaEquivalence.isEquivalent(lemma, rightLemma)
            && constrainedVariables == rightConstrainedRenamed
            && postVariables == rightPostRenamed)
      }

  }
}

object AnnotatedLemma {
  /** Refine an annotated lemma and return the refined annotated lemma. If the refinement is
    * undefined for the input lemma, return None.
    */
  def refine(problem: Problem,
             annotatedLemma: AnnotatedLemma,
             refinement: Refinement): Option[AnnotatedLemma] = {
    refinement.refine(problem, annotatedLemma.lemma) match {
      case None => None
      case Some(refinedLemma) =>
        // calculate constrained and post variables for refined annotated lemma
        val newPost = calculatePostVariables(annotatedLemma.postVariables, refinement)
        val newConstrained = calculateConstrainedVariables(annotatedLemma.constrainedVariables, refinement)
        Some(AnnotatedLemma(refinedLemma, newConstrained, newPost))
    }
  }

  /** Replace all occurrences of `element` in `set` with `replacement`. */
  private def replace(set: Set[MetaVar], element: MetaVar, replacement: MetaVar): Set[MetaVar] = {
    set.map(mv =>
      if(element == mv)
        replacement
      else
        mv
    )
  }

  /** Calculate a new set of post variables, given the current post variables and a refinement. */
  def calculatePostVariables(post: Set[MetaVar], refinement: Refinement): Set[MetaVar] = {
    refinement match {
      case Predicate(fn, args) => post
      case SuccessfulApplication(fn, args, result) => post + result
      case Equation(left, right) => replace(post, right, left)
    }
  }

  /** Calculate a new set of constrained variables, given the current constrained variables and a refinement */
  def calculateConstrainedVariables(constrainedVariables: Set[MetaVar], refinement: Refinement): Set[MetaVar] = {
    refinement match {
      case Predicate(fn, args) =>
        constrainedVariables ++ args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar]))
      case SuccessfulApplication(fn, args, result) =>
        constrainedVariables ++ args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar])) + result
      case Equation(left, right) => replace(constrainedVariables, right, left)
    }
  }
}