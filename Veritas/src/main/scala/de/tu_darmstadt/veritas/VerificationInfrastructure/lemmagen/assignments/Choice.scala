package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

sealed trait Choice
case class VariableChoice(mv: MetaVar) extends Choice
case class FreshChoice(typ: SortRef) extends Choice

object Choice {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Constraint._

  def generate(constraint: Constraint, bound: Set[MetaVar]): Set[Choice] = constraint match {
    case Fresh(typ) => Set(FreshChoice(typ))//
    case Fixed(mv) => Set(VariableChoice(mv))
    case Bound(typ) => bound.filter(_.sortType == typ).map(VariableChoice).toSet
    case Prefer(primary, secondary) =>
      val primaryChoices = generate(primary, bound)
      if(primaryChoices.nonEmpty)
        primaryChoices
      else
        generate(secondary, bound)
    case Union(of) => of.flatMap(generate(_, bound))
    case Exclude(base, without) => generate(base, bound) -- generate(without, bound)
  }

  def extractVariableChoices(choices: Seq[Choice]): Seq[MetaVar] = choices.collect {
    case VariableChoice(mv) => mv
  }
}
