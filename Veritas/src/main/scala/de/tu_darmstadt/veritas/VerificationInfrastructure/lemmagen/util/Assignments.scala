package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Constraint.Constraint
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
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

  def generate(constraints: Seq[Constraint], lemma: Lemma): Seq[Seq[MetaVar]] = {
    generate(constraints, lemma.boundVariables)
  }

  def generate(constraints: Seq[Constraint], boundVariables: Set[MetaVar] = Set()): Seq[Seq[MetaVar]] = {
    generateAbstractAssignments(constraints, boundVariables)
      .map(generateConcreteAssignment(_, boundVariables))
  }

  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))

  def generateSimple(types: Seq[SortRef], lemma: Lemma): Seq[Seq[MetaVar]] = {
    generateSimple(types, lemma.boundVariables)
  }

  def generateSimple(types: Seq[SortRef], boundVariables: Set[MetaVar] = Set()): Seq[Seq[MetaVar]] = {
    generate(Constraint.preferBound(types), boundVariables)
  }

  def generateSimpleSingle(types: Seq[SortRef]): Seq[MetaVar] = {
    generateSimple(types).head
  }
}
