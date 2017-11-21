package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.backend.ast.function.FunctionSig
import de.tu_darmstadt.veritas.backend.ast.{Functions, ModuleDef, SortRef}

trait DomainSpecificKnowledge {
  object ADTBase extends Enumeration {
    val EXPRESSION, CONTEXT, TYP = Value
  }

  def simpleRecursiveFunctions(): Functions
  def isSimpleRecursiveFunction(name: String): Boolean

  // TODO: did you meant to annotate axioms and lemmas? To annotate function with such information doesnt make any sense
  // contains axioms and lemmas
  def staticDomain(): Seq[ModuleDef]
  def isStaticDomain(name: String): Boolean

  // contains axioms and lemmas
  def dynamicDomain(): Seq[ModuleDef]
  def isDynamicDomain(name: String): Boolean

  // returns for a cotr name or a base type name which base type it has
  // some adts can belong to multiple bases
  def getADTBase(name: String): Seq[ADTBase.Value]
  // returns for a cotr name or a base type name if it belongs to context
  def isContext(name: String): Boolean
  def isExpression(name: String): Boolean
  def isTyp(name: String): Boolean

  // transforms signature in such a way that we can see what params belong
  // to Expression, Context and Typ. The same goes for the return type
  def transformFunctionSig(sig: FunctionSig): (Seq[ADTBase.Value], ADTBase.Value)
  // Returns position and the type
  // Sadly we cannot return the name because in the Veritas ast format
  // we do not work with names of parameters only with the position
  def getTransformedParameters(functionName: String): Seq[(Int, SortRef)]
  def isTransformedParameter(functionName: String, position: Int): Boolean
}