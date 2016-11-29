package system

import system.Syntax.{ISort, Rule, Sort, Symbol}

import scala.collection.immutable.ListMap

case class Language(name: String, sorts: Seq[_ <: ISort], syms: Seq[Symbol], rules: Seq[Rule]) {
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

  val closedDataTypes: ListMap[Sort, Seq[Symbol]] = {
    val types = sorts.flatMap(s => if (s.isInstanceOf[Sort] && !s.open) Some(s.asInstanceOf[Sort]) else None)
    ListMap() ++ types.map(s => s -> syms.filter(sym => sym.constr && sym.out == s))
  }

  val openDataTypes: Seq[ISort] =
    sorts.filter(_.open)

  val funSymbols: Seq[Symbol] = {
    val constrs = closedDataTypes.values.flatten.toSeq
    syms.diff(constrs)
  }

  def +(lang: Language): Language = {
    val name = this.name
    var sorts = this.sorts
    var syms = this.syms
    var rules = this.rules

    for (s <- lang.sorts if !this.sorts.contains(s))
      sorts :+= s
    for (s <- lang.syms if !this.syms.contains(s))
      syms :+= s
    for (r <- lang.rules if !this.rules.contains(r))
      rules :+= r

    Language(name, sorts, syms, rules)
  }

  def +(trans: Transformation): Language = {
    val base = this + trans.lang

    val name = base.name + "-" + trans.contractedSym
    val sorts = base.sorts
    val syms = base.syms :+ trans.contractedSym
    val rules = base.rules :+ trans.contract
    Language(name, sorts, syms, rules)
  }

}
