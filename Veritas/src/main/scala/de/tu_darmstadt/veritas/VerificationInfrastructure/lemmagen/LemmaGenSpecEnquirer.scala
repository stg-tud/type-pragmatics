package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledge

class LemmaGenSpecEnquirer(spec: Module, dsk: VeritasDomainSpecificKnowledge) extends VeritasSpecEnquirer(spec) {
  /** Return SortRefs to all data types that have at least one constructor involving typ */
  def functions: Set[FunctionDef] = (dsk.staticFunctions ++ dsk.dynamicFunctions).diff(predicates)
  def predicates: Set[FunctionDef] = dsk.predicates
  def dataTypes = tdcollector.dataTypes

  def staticFunctions: Set[FunctionDef] = dsk.staticFunctions
  def dynamicFunctions: Set[FunctionDef] = dsk.dynamicFunctions

  def getDataTypesInvolving(typ: SortRef): Seq[SortRef] = {
    tdcollector.dataTypes
      .filter({ case (_, (_, constructors)) => constructors.exists(_.in.contains(typ)) })
      .keys
      .map(SortRef(_))
      .toSeq
  }

  /** Retrieve all boolean functions that take any of `types` */
  def retrievePredicates(types: Set[SortRef]): Set[FunctionDef] =
    types.flatMap(typ => predicates.filter(_.signature.in.contains(typ)))

  /** Return all static and dynamic functions that take any of `types` */
  def retrieveTransformers(types: Set[SortRef]): Set[FunctionDef] =
    types.flatMap(typ => functions.filter(_.signature.in.contains(typ)))

  /**
    * Return all static and dynamic functions that produce any type involving
    * any of `types`, or any member of `types` itself
    * */
  def retrieveProducers(types: Set[SortRef]): Set[FunctionDef] = {
    types.flatMap(typ => {
      val involvingTyp = typ +: getDataTypesInvolving(typ)
      functions.filter(involvingTyp contains _.signature.out)
    })
  }

  def isFailableType(typ: SortRef): Boolean = dsk.failableTypes.exists(_.name == typ.name)

  /** Return the constructors of a failable type. Assumes there are exactly two constructors, one
    * without parameters (= fail), another with one parameter. Fail otherwise.
    */
  def retrieveFailableConstructors(typ: SortRef): (DataTypeConstructor, DataTypeConstructor) = {
    val constructors = dsk.failableTypes.find(_.name == typ.name).get.constrs
    if(constructors.length != 2)
      sys.error(s"assumed two constructors for failable type ${typ}")
    (constructors.find(_.in.isEmpty).get, constructors.find(_.in.length == 1).get)
  }
}
