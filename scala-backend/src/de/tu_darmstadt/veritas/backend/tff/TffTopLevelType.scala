package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.fof.FunSymbol


trait TffTopLevelType extends SimplePrettyPrintable

trait TffAtomicType extends TffTopLevelType

/*
 * For types with dollars, including the predefined ones ($oType, $o ....) 
 */
final case class DefinedType(name: String) extends TffAtomicType {
  override def prettyString = "$" + name
}

// this currently only supports types without arguments!!
final case class SymbolType(name: TypedSymbol) extends TffAtomicType {
  override def prettyString = name.toUntyped.prettyString
}


//TODO do we need variable types/polymorphic types as well?? currently not supported!


final case class TffMappingType(argslist: Seq[TffAtomicType], restype: TffAtomicType) extends TffTopLevelType {
  require(argslist.length >= 1, "TffMappingType needs at least one argument type!")
  
  override def prettyString = {
    val argsliststr = argslist map (a => a.prettyString)
    if (argsliststr.length == 1)
      argsliststr(0) + " > " + restype.prettyString
    else
      argsliststr.mkString("(", " * ", ") > ") + restype.prettyString
  }
}


