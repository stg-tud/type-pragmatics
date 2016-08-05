package de.tu_darmstadt.veritas.inputdsl

/**
 * meta variables as placeholders for elements of a certain type
 */
abstract class MetaVar(name: String) extends SpecConstruct(name) {
  
  //TODO improve check?
  override def checkConsistency(): Boolean = true

}

/**
 * meta variables that remain underspecified (must not have a definition)
 * assumption: infinitely large domain (corresponds to open datatype from previous Veritas language)
 */
case class UnderspecifiedMetaVar(name: String) extends MetaVar(name)

/**
 * meta variables with a specification (must be defined within a syntax block)
 * assumption: closed domain, i.e. the definition of the meta variable defines all forms
 * which expressions of this variable can ever have (corresponds to closed datatype from previous Veritas language)
 */
case class SpecifiedMetaVar(name: String) extends MetaVar(name)


