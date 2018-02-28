package de.tu_darmstadt.veritas.newinputdsl.lang

import scala.annotation.Annotation

trait SPLDomainSpecificKnowledgeAnnotations {

  case class Property() extends Annotation
  // a function can have multiple properties and each property gets a name assigned and a function
  case class PropertyAttached(functionName: String) extends Annotation
  // trade-off to refer by position number because we cannot annotate a case
  case class PropertyNeeded(propertyName: String, functionEquationPositions: Int*) extends Annotation {
    require(functionEquationPositions.nonEmpty)
  }

  // TODO what happens if reduce semantics takes two arguments that need to be reduced?
  // => One solution would be that designers would be need a tuple adt
  // first position function param pos, second ctor position of function param and so on
  case class Recursive(positions: Int*) extends Annotation {
    require(positions.nonEmpty)
  }

  // check what is the common factor in those groups and create a case distinction for them
  // for example outer cotrs (qsingle, qseq, qcond)
  case class GroupedDistinction(positions: Seq[Int]*) extends Annotation {
    val atLeastOneGroup = positions.nonEmpty
    val allGroupsNonEmpty = positions.seq.forall { group => group.nonEmpty }
    require(atLeastOneGroup && allGroupsNonEmpty)
  }

  // Used to mark a function that is referenced by a distinction annotation
  case class DistinctionCriteria() extends Annotation

  // TODO how do we pass the distinction criteria?
  // these distinctions are equalities/inequalities
  // one possiblity could be defs which are referenced by name
  // TODO what if the distinction represents a grouping which were marked by GroupedDistinction?
  // just point to one of the elements in the grouping or the grouping itself?
  // By checking if a position is already grouped we know that the distinction has to be applied to a subobligation of
  // a structural distinction
  case class Distinction(criteriaName: String, positions: Int*) extends Annotation

  // TODO how do we make a lemma app in a sub of a distinction?
  // by progress/lemma needed? and determine which criteria fits the function pattern?
}

object SPLDomainSpecificKnowledgeAnnotations {
  val annotationsIngoringFunction: Seq[String] =
    Seq("DistinctionCriteria", "Property")
}
