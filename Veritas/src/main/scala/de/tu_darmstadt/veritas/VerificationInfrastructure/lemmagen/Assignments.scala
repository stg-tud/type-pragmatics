package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}

object Assignments {
  def generateAssignments(lemma: Lemma, types: Seq[SortRef],
                          prefix: Seq[MetaVar] = Seq()): Seq[Seq[MetaVar]] = types match {
    case Nil => Seq(prefix)
    case head :: tail =>
      // use bound if there are variables of that type. TODO
      val choices = lemma.bindingsOfType(head).toSeq
      if(choices.isEmpty) {
        // bind a new variable
        val mv = FreshVariables.freshMetaVar(lemma.freeVariables ++ prefix.toSet, head)
        generateAssignments(lemma, tail, prefix :+ mv)
      } else {
        choices.flatMap(mv => generateAssignments(lemma, tail, prefix :+ mv))
      }
  }

  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))
}
