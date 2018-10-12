package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.{SortRef, VeritasConstruct}
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledge

class LemmaGenSpecEnquirer(spec: VeritasConstruct, dsk: DomainSpecificKnowledge) extends VeritasSpecEnquirer(spec) {
  /** Return SortRefs to all data types that have at least one constructor involving typ */
  def functions: Set[FunctionDef] = dsk.staticFunctions ++ dsk.dynamicFunctions
  def predicates: Set[FunctionDef] = dsk.predicates

  def getDataTypesInvolving(typ: SortRef): Seq[SortRef] = {
    tdcollector.dataTypes
      .filter({ case (_, (_, constructors)) => constructors.exists(_.in.contains(typ)) })
      .keys
      .map(SortRef(_))
      .toSeq
  }
  /** Retrieve all boolean functions that take typ */
  def retrievePredicates(typ: SortRef): Set[FunctionDef] = predicates.filter(_.signature.in.contains(typ))

  /** Return all static and dynamic functions that take typ */
  def retrieveTransformers(typ: SortRef): Set[FunctionDef] = functions.filter(_.signature.in.contains(typ))

  /** Return all static and dynamic functions that produce a type involving typ, or typ itself */
  def retrieveProducers(typ: SortRef): Set[FunctionDef] = {
    val involvingTyp = typ +: getDataTypesInvolving(typ)
    functions.filter(involvingTyp contains _.signature.out)
  }
}
