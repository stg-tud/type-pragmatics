package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

object Constraint {
  sealed trait Constraint
  case class Bound(typ: SortRef) extends Constraint
  case class Fresh(typ: SortRef) extends Constraint
  case class Fixed(mv: MetaVar) extends Constraint
  case class Prefer(primary: Constraint, secondary: Constraint) extends Constraint
  case class Union(of: Set[Constraint]) extends Constraint
  case class Exclude(base: Constraint, without: Constraint) extends Constraint

  def fresh(types: Seq[SortRef]): Seq[Constraint] = types.map(fresh)
  def fresh(typ: SortRef): Constraint = Fresh(typ)

  def bound(types: Seq[SortRef]): Seq[Constraint] = types.map(Bound)

  def freshOrBound(typ: SortRef): Constraint = Union(Set(Bound(typ), Fresh(typ)))
  def freshOrBound(types: Seq[SortRef]): Seq[Constraint] = types.map(freshOrBound)

  def preferBound(typ: SortRef): Constraint = Prefer(Bound(typ), Fresh(typ))
  def preferBound(types: Seq[SortRef]): Seq[Constraint] = types.map(preferBound)
}