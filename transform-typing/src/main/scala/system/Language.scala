package system

import system.Syntax.{ISort, Rule, Sort, Symbol}

case class Language(name: String, sorts: Set[_ <: ISort], syms: Set[Symbol], rules: Set[Rule]) {
  override def toString: String = {
    s"""sorts
        |${sorts.mkString(", ")}
        |
         |symbols
        |${syms.map(_.sigString).mkString("\n")}
        |
         |rules
        |${rules.mkString("\n\n")}
       """.stripMargin
  }

  def closedDataTypes: Map[Sort, Set[Symbol]] = {
    val types = sorts.flatMap(s => if (s.isInstanceOf[Sort] && !s.open) Some(s.asInstanceOf[Sort]) else None)
    types.map(s => s -> syms.filter(sym => sym.constr && sym.out == s)).toMap
  }

  def openDataTypes: Set[Sort] =
    sorts.flatMap(s => if (s.isInstanceOf[Sort] && s.open) Some(s.asInstanceOf[Sort]) else None)
}
