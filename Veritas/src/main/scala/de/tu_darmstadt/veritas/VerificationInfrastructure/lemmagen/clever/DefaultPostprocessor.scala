package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.{ExistsJudgment, FunctionExpJudgment, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionExpNeq, FunctionMeta}

class DefaultPostprocessor(problem: Problem) extends Postprocessor {
  implicit private val enquirer = problem.enquirer

  def reformulateProgressLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.map(lemma =>
      lemma.consequences.head match {
        case FunctionExpJudgment(FunctionExpNeq(l, FunctionExpApp(failConstructorName,  Seq()))) =>
          // find out return type from AST
          val outType = enquirer.dataTypes.collect {
            case (name, (_, constructors)) if constructors.exists(_.name == failConstructorName) => SortRef(name)
          }.head
          val successfulOutType = enquirer.retrieveFailableConstructors(outType)._2.in.head
          val (_, successConstructor) = enquirer.retrieveFailableConstructors(outType)
          val assignments = Assignments.generate(Seq(Constraint.fresh(successfulOutType)), lemma.boundVariables)
          val successVar = assignments.head.head
          val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
          val equality = enquirer.makeEquation(l, successExp).asInstanceOf[FunctionExpJudgment]
          val exists = ExistsJudgment(Seq(successVar), Seq(equality))
          new Lemma(lemma.name, lemma.premises, Seq(exists), lemma.refinements)
        case _ => lemma
      })
  }

  def renameLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.map { lemma =>
      val suffix = f"${lemma.hashCode()}%08X"
      new Lemma(s"${lemma.name}_$suffix", lemma.premises, lemma.consequences, lemma.refinements)
    }
  }

  override def process(lemmas: Seq[Lemma]): Seq[Lemma] = {
    // first, reformulate progress lemmas
    val reformulated = reformulateProgressLemmas(lemmas)
    // then, rename lemmas
    renameLemmas(reformulated)
  }
}
