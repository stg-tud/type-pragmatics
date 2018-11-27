package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}

object Assignments {
  def generateAssignments(lemma: Lemma, types: Seq[SortRef]): Seq[Seq[MetaVar]] = {
    placeVariables(lemma, preferBoundPlacements(types))
  }

  def wrapMetaVars(seq: Seq[MetaVar]): Seq[FunctionExpMeta] = seq.map(mv => FunctionMeta(mv))

  sealed trait Placement
  case class Bound(typ: SortRef) extends Placement()
  case class Fresh(typ: SortRef) extends Placement()
  case class Fixed(mv: MetaVar) extends Placement()
  case class Prefer(`try`: Placement, `else`: Placement) extends Placement()
  case class Union(of: Set[Placement]) extends Placement()

  def generatePlacementChoice(lemma: Lemma, placement: Placement,
                              prefix: Seq[MetaVar], bound: Set[MetaVar]): Set[MetaVar] = placement match {
    case Fresh(typ) => Set(FreshVariables.freshMetaVar(lemma.freeVariables ++ prefix.toSet, typ))
    case Fixed(mv) => Set(mv)
    case Bound(typ) => lemma.bindingsOfType(typ) ++ (prefix ++ bound).filter(_.sortType == typ)
    case Prefer(tryPlacement, elsePlacement) =>
      val tryChoices = generatePlacementChoice(lemma, tryPlacement, prefix, bound)
      if(tryChoices.isEmpty)
        generatePlacementChoice(lemma, elsePlacement, prefix, bound)
      else
        tryChoices
    case Union(of) => of.flatMap(generatePlacementChoice(lemma, _, prefix, bound))
  }

  def placeVariables(lemma: Lemma, placements: Seq[Placement],
                     prefix: Seq[MetaVar] = Seq(),
                     bound: Set[MetaVar] = Set()): Seq[Seq[MetaVar]] = placements match {
    case Nil => Seq(prefix)
    case head :: tail =>
      // use bound if there are variables of that type.
      val choice = generatePlacementChoice(lemma, head, prefix, bound)
      choice.flatMap(mv => placeVariables(lemma, tail, prefix :+ mv, bound)).toSeq
      // TODO: Fresh may bind variables that are fixed somewhere
  }

  def freshPlacements(types: Seq[SortRef]): Seq[Placement] = types.map(Fresh)
  def boundPlacements(types: Seq[SortRef]): Seq[Placement] = types.map(Bound)
  def freshOrBoundPlacements(types: Seq[SortRef]): Seq[Placement] = types.map(typ => Union(Set(Bound(typ), Fresh(typ))))
  def preferBoundPlacements(types: Seq[SortRef]): Seq[Placement] = types.map(typ =>
    Prefer(Bound(typ), Fresh(typ))
  )
}