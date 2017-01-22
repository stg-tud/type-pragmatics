package system

import system.Syntax.{ISort, Rule, Sort, Symbol}

import scala.collection.immutable.ListMap

case class Language(name: String, sorts: Seq[_ <: ISort], syms: Seq[Symbol], rules: Seq[Rule], transs: Seq[Transformation] = Seq()) {
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

  val undeclaredSymbols: Set[Symbol] = {
    val rsyms = rules.foldLeft(Set[Symbol]())((set, r) => set ++ r.symbols)
    val transSyms = transs.flatMap(t => t.contractedSym +: t.extraSymbols).toSet
    val transUndeclared = transs.flatMap(_.undeclaredSymbols).toSet
    (rsyms ++ transUndeclared).diff(syms.toSet).diff(transSyms)
  }

  def +(trans: Transformation): Language = {
//    val name = this.name + "+" + trans.contractedSym
//    val sorts = this.sorts
//    val syms = this.syms :+ trans.contractedSym
//    val rules = this.rules :+ trans.contract

    var newtranss = transs
    for (t <- trans.lang.transs if !transs.contains(t))
      newtranss :+= t
    newtranss :+= trans

    Language(name + "+" + trans.contractedSym, sorts, syms, rules, newtranss)
  }

}
