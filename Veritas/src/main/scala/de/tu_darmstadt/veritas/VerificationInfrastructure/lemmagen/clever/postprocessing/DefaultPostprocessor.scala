package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionExpNeq, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast.{ExistsJudgment, FunctionExpJudgment, SortRef}

/** Postprocess lemmas:
  * Reformulate progress lemmas, sort the premises of lemmas, generate unique names for lemmas
  * and finally return them as a lexicographically sorted sequence.
  */
class DefaultPostprocessor(problem: Problem) extends Postprocessor {
  implicit private val enquirer = problem.enquirer

  /** Return a modification of `lemmas`, with all progress lemmas being reformulated
    * such that their consequence contains an existential formula.
    */
  def reformulateProgressLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.map(lemma =>
      lemma.consequences.head match {
        case FunctionExpJudgment(FunctionExpNeq(l, FunctionExpApp(failConstructorName,  Seq()))) =>
          // find out return type from AST
          val outType = enquirer.dataTypes.collect {
            case (name, (_, constructors)) if constructors.exists(_.name == failConstructorName) => SortRef(name)
          }.head
          // construct a new consequence
          val successfulOutType = enquirer.retrieveFailableConstructors(outType)._2.in.head
          val (_, successConstructor) = enquirer.retrieveFailableConstructors(outType)
          val assignments = Assignments.generate(Seq(Constraint.fresh(successfulOutType)), lemma.boundVariables)
          val successVar = assignments.head.head
          val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
          val equality = enquirer.makeEquation(l, successExp).asInstanceOf[FunctionExpJudgment]
          val exists = ExistsJudgment(Seq(successVar), Seq(equality))
          new Lemma(lemma.name, lemma.premises, Seq(exists))
        case _ => lemma
      })
  }

  /** Return a modification of `lemmas`, in which each lemma is assigned a unique name derived from its hash code  */
  def renameLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.map { lemma =>
      val code = lemma.hashCode
      val suffix = f"${code}%08X"
      lemma.rename(s"${lemma.name}$suffix")
    }
  }

  /** Return a modification of `lemmas`, in which the premises of each lemma are sorted lexicographically. */
  def sortPremises(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.map { lemma =>
      new Lemma(lemma.name, lemma.premises.sortBy(_.toString.stripPrefix("(").stripPrefix("~")), lemma.consequences)
    }
  }

  override def process(graph: RefinementGraph): Seq[Lemma] = {
    val lemmas = graph.selectedNodes.map(_.lemma).toSeq
    // first, reformulate progress lemmas
    // then, sort premises
    // then, rename lemmas
    // then, sort by name
    (reformulateProgressLemmas _)
      .andThen(sortPremises)
      .andThen(renameLemmas)
      .andThen(lemmas => lemmas.sortBy(_.name))(lemmas)
  }
}
