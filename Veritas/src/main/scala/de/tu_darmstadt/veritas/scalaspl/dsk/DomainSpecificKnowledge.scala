package de.tu_darmstadt.veritas.scalaspl.dsk

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionSig}

trait DomainSpecificKnowledge {
  def failableTypes: Seq[DataType]
  def recursiveFunctions: Map[FunctionDef, DataType]
  def progressProperties: Map[FunctionDef, Set[TypingRule]]
  def preservationProperties: Map[FunctionDef, Set[TypingRule]]

  def properties: Set[TypingRule]
  def staticFunctions: Set[FunctionDef]
  def dynamicFunctions: Set[FunctionDef]
  def predicates: Set[FunctionDef]

  def lookupByFunName[T](mp: Map[FunctionDef, Set[T]], funname: String): Iterable[T] = {
    val allkeys: Iterable[FunctionDef] = mp.keys.filter { fd: FunctionDef => fd match {
      case FunctionDef(FunctionSig(name, _, _), _) => name == funname
    }}
    allkeys.flatMap(mp)
  }
}
