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

  val allSyms = transs.foldLeft(syms)((seq, t) => t.contractedSym +: seq)

  def allRules = transs.foldLeft(rules)((seq, t) => (seq :+ t.contract._1) ++ t.lemmas.keys)

  val closedDataTypes: ListMap[Sort, Seq[Symbol]] = {
    val types = sorts.flatMap(s => if (s.isInstanceOf[Sort] && !s.abstractEnum) Some(s.asInstanceOf[Sort]) else None)
    ListMap() ++ types.map(s => s -> allSyms.filter(sym => sym.constr && sym.out == s))
  }

  val openDataTypes: ListMap[ISort, Seq[Symbol]] = {
    val types = sorts.filter(_.abstractEnum)
    ListMap() ++ types.map(s => s -> allSyms.filter(sym => sym.constr && sym.out == s))
  }

  val funSymbols: Seq[Symbol] = {
    val constrs = closedDataTypes.values.flatten.toSeq ++ openDataTypes.values.flatten
    syms.diff(constrs)
  }

  lazy val undeclaredSymbols: Set[Symbol] = {
    val rsyms = rules.foldLeft(Set[Symbol]())((set, r) => set ++ r.symbols)
    val transSyms = transs.map(t => t.contractedSym).toSet
    val transUndeclared = transs.flatMap(_.undeclaredSymbols).toSet
    (rsyms ++ transUndeclared).diff(syms.toSet).diff(transSyms)
  }

  def +(trans: Transformation): Language = {
    var newtranss = transs
    for (t <- trans.lang.transs if !transs.contains(t))
      newtranss :+= t
    newtranss :+= trans

    Language(name + "+" + trans.contractedSym, sorts, syms, rules, newtranss)
  }

  def +(ext: LanguageExtension): Language =
    Language(name + "+" + ext.name, sorts ++ ext.sorts, syms ++ ext.syms, rules ++ ext.rules, transs)
}

case class LanguageExtension(name: String, sorts: Seq[_ <: ISort], syms: Seq[Symbol], rules: Seq[Rule])
