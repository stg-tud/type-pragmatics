package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

sealed trait Choice
case class VariableChoice(mv: MetaVar) extends Choice
case class FreshChoice(typ: SortRef) extends Choice

/** Assume we have a set of available variables (along with their types) and a
  * constraint (see Constraint).
  * This object then generates a set of choices for the constraint.
  * A choice is one of the following:
  *
  *   VariableChoice(mv), which means that the variable `mv` can be chosen
  *   FreshChoice(typ), which means that a fresh variable of type `type` can be chosen
  */
object Choice {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Constraint._

  def generate(constraint: Constraint, bound: Set[MetaVar]): Set[Choice] = constraint match {
    case Fresh(typ) => Set(FreshChoice(typ))
    case Fixed(mv) => Set(VariableChoice(mv))
    case Bound(typ) => // any bound variable of matching type
      bound.filter(_.sortType == typ).map(VariableChoice).toSet
    case Prefer(primary, secondary) => // if possible, choose `primary`, otherwise `secondary`
      val primaryChoices = generate(primary, bound)
      if(primaryChoices.nonEmpty)
        primaryChoices
      else
        generate(secondary, bound)
    case Union(of) => // any of `of`
      of.flatMap(generate(_, bound))
  }

  /** Return all VariableChoice choices from `choices` */
  def extractVariableChoices(choices: Seq[Choice]): Seq[MetaVar] = choices.collect {
    case VariableChoice(mv) => mv
  }
}
