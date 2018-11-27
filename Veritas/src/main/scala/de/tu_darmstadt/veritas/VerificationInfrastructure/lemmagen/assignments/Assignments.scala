package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Constraint.Constraint
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{FreshVariables, Lemma}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

import scala.collection.mutable

object Assignments {
  def generateAbstractAssignments(constraints: Seq[Constraint], bound: Set[MetaVar],
                                  prefix: Seq[Choice] = Seq()): Seq[Seq[Choice]] = constraints match {
    case Nil => Seq(prefix)
    case head :: tail =>
      // use bound if there are variables of that type.
      val choice = Choice.generate(head, bound ++ Choice.extractVariableChoices(prefix).toSet)
      choice.flatMap(mv => generateAbstractAssignments(tail, bound, prefix :+ mv)).toSeq
  }

  def generateConcreteAssignment(assignment: Seq[Choice], bound: Set[MetaVar]): Seq[MetaVar] = {
    val variableChoices: Set[MetaVar] = assignment.collect {
      case VariableChoice(mv) => mv
    }.toSet
    var freshlyBound = new mutable.MutableList[MetaVar]()
    assignment.map {
      case VariableChoice(mv) => mv
      case FreshChoice(typ) =>
        val mv = FreshVariables.freshMetaVar(variableChoices ++ bound ++ freshlyBound, typ)
        freshlyBound += mv
        mv
    }
  }

  def generate(lemma: Lemma, constraints: Seq[Constraint]): Seq[Seq[MetaVar]] = {
    generateAbstractAssignments(constraints, lemma.boundVariables)
      .map(generateConcreteAssignment(_, lemma.boundVariables))
  }

  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))

  def generateSimple(lemma: Lemma, types: Seq[SortRef]): Seq[Seq[MetaVar]] = {
    generate(lemma, Constraint.preferBound(types))
  }
}
