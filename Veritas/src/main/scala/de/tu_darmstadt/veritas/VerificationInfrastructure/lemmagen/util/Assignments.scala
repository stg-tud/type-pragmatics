package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Constraint.Constraint
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

import scala.collection.mutable

/** Helper object for choosing variables from a set of available variables according to some constraints.
  * See AssignmentsTest for some examples. */
object Assignments {
  /** Given a sequence of constraints and a set of available (bound) variables,
    * find the set of legal choices (see Choice) for each constraint and then
    * construct all sequences which fulfill all constraints.
    */
  def generateAbstractAssignments(constraints: Seq[Constraint], bound: Set[MetaVar],
                                  prefix: Seq[Choice] = Seq()): Seq[Seq[Choice]] = constraints match {
    case Nil => Seq(prefix)
    case head :: tail =>
      // generate all choices for `head`
      val choice = Choice.generate(head, bound ++ Choice.extractVariableChoices(prefix).toSet)
      choice.flatMap(mv => generateAbstractAssignments(tail, bound, prefix :+ mv)).toSeq
  }

  /** Given a sequence of choices and a set of available (bound) variables, return a
    * sequence of concrete variables, i.e. a concrete assignment.
    */
  def generateConcreteAssignment(assignment: Seq[Choice], bound: Set[MetaVar]): Seq[MetaVar] = {
    // we first extract all exact variable choices
    val variableChoices: Set[MetaVar] = assignment.collect {
      case VariableChoice(mv) => mv
    }.toSet
    // we store all fresh variables we have generated so far to avoid name clashes
    var freshlyBound = new mutable.MutableList[MetaVar]()
    assignment.map {
      case VariableChoice(mv) => mv
      case FreshChoice(typ) =>
        val mv = FreshVariables.freshMetaVar(variableChoices ++ bound ++ freshlyBound, typ)
        freshlyBound += mv
        mv
    }
  }

  /** Generate a sequence of concrete assignments that fulfill the given constraints, assuming
    * the bound variables of `lemma`. */
  def generate(constraints: Seq[Constraint], lemma: Lemma): Seq[Seq[MetaVar]] = {
    generate(constraints, lemma.boundVariables)
  }

  /** Generate a sequence of concrete assignments that fulfill the given constraints,
    * assuming a set of bound variables. */
  def generate(constraints: Seq[Constraint], boundVariables: Set[MetaVar] = Set()): Seq[Seq[MetaVar]] = {
    generateAbstractAssignments(constraints, boundVariables)
      .map(generateConcreteAssignment(_, boundVariables))
  }

  /** Convert a sequence of MetaVar instances to a sequence of FunctionExpMeta instances. */
  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))

  /** Generate a sequence of concrete assignments which match the types in `types`.
    * For each position, we prefer to pass the bound variables of `lemma`, but if
    * there are no such bound variables, we pass a fresh variable symbol.
    */
  def generateSimple(types: Seq[SortRef], lemma: Lemma): Seq[Seq[MetaVar]] = {
    generateSimple(types, lemma.boundVariables)
  }

  /** Generate a sequence of concrete assignments which match the types in `types`.
    * For each position, we prefer to pass the bound variables in `boundvariables`,
    * but if there are no such bound variables, we pass a fresh variable symbol.
    */
  def generateSimple(types: Seq[SortRef], boundVariables: Set[MetaVar] = Set()): Seq[Seq[MetaVar]] = {
    generate(Constraint.preferBound(types), boundVariables)
  }

  /** Generate exactly one concrete assignments which match the types in `types`.
    * For each position, we prefer to pass the bound variables of `lemma`, but if
    * there are no such bound variables, we pass a fresh variable symbol.
    */
  def generateSimpleSingle(types: Seq[SortRef]): Seq[MetaVar] = {
    generateSimple(types).head
  }
}
