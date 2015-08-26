package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

trait TffAtomicType extends SimplePrettyPrintable

/*
 * For types with dollars, including the predefined ones ($oType, $o ....) 
 */
final case class DefinedType(name: String) extends TffAtomicType with SimplePrettyPrintable {
  override def prettyString = "$" + name
}

// this currently only supports types without arguments!!
final case class SymbolType(name: String) extends TffAtomicType with SimplePrettyPrintable {
  override def prettyString = name
}


//TODO do we need variable types/polymorphic types as well??

