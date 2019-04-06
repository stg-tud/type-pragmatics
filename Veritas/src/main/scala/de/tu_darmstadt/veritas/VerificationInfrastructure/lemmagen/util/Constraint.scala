package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}

/** Assume we have a set of available variables (e.g. the bound variables of a lemma)
  * and need to pass n variables of some types to a function.
  * We can use Constraints to extract all possible choices of variables.
  * A constraint models a restriction on the possible choice of variables, e.g.
  * "Choose any available variable of sort X"
  * "Generate a fresh variable of sort X"
  */
object Constraint {
  sealed trait Constraint
  // choose any bound variable of matching type
  case class Bound(typ: SortRef) extends Constraint
  // generate a fresh variable symbol of matching type
  case class Fresh(typ: SortRef) extends Constraint
  // choose exactly `mv`
  case class Fixed(mv: MetaVar) extends Constraint
  // try to choose `primary`, but if there are no choices, choose `secondary`
  case class Prefer(primary: Constraint, secondary: Constraint) extends Constraint
  // choose any of `of`
  case class Union(of: Set[Constraint]) extends Constraint

  // some abbreviations
  def fixed(mv: MetaVar): Constraint = Fixed(mv)

  def fresh(types: Seq[SortRef]): Seq[Constraint] = types.map(fresh)
  def fresh(typ: SortRef): Constraint = Fresh(typ)

  def bound(types: Seq[SortRef]): Seq[Constraint] = types.map(Bound)

  def freshOrBound(typ: SortRef): Constraint = Union(Set(Bound(typ), Fresh(typ)))
  def freshOrBound(types: Seq[SortRef]): Seq[Constraint] = types.map(freshOrBound)

  def preferBound(typ: SortRef): Constraint = Prefer(Bound(typ), Fresh(typ))
  def preferBound(types: Seq[SortRef]): Seq[Constraint] = types.map(preferBound)
}