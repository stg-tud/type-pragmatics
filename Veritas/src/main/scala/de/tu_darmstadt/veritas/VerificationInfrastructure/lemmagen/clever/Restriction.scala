package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Assignments
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem, Refinement}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionMeta}

trait Restriction {
  def restrict(problem: Problem, lemma: Lemma): Lemma
}

object Restriction {
  case class Equality(variables: Set[MetaVar]) extends Restriction {
    def restrict(problem: Problem, lemma: Lemma): Lemma = {
      if(variables.nonEmpty) {
        val pivot = variables.head
        val rest = variables.tail
        val refinements = rest.map(mv => Refinement.Equation(pivot, FunctionMeta(mv)))
        refinements.foldLeft(lemma)((l, r) => r.refine(problem, l).getOrElse(l))
      } else {
        lemma
      }
    }
  }

  case class Application(fn: FunctionDef, args: Seq[MetaVar], result: MetaVar) extends Restriction {
    def restrict(problem: Problem, lemma: Lemma): Lemma = {
      val refinement = Refinement.SuccessfulApplication(fn, Assignments.wrapMetaVars(args), result)
      refinement.refine(problem, lemma).getOrElse(lemma)
    }
  }
}
