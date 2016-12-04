import system.Language
import system.Syntax._

package object stlc {
  private var sorts = Seq[ISort]()
  def sort(name: String): Sort =
    sort(Sort(name)).asInstanceOf[Sort]
  def sort(sort: ISort): ISort =
    if (!initialized) {
      if (!sorts.contains(sort))
        sorts :+= sort
      sort
    }
    else
      throw new RuntimeException(s"Cannot register sort $sort because language was already initialized")

  private var syms = Seq[Symbol]()
  def symbol(name: String, in: List[ISort], out: ISort): Symbol =
    symbol(Symbol(name, in, out))
  def symbol(sym: Symbol): Symbol =
    if (!initialized) {
      if (!syms.contains(sym))
        syms :+= sym
      sym
    }
    else
      throw new RuntimeException(s"Cannot register symbol $sym because language was already initialized")

  private var rules = Seq[Rule]()
  def rule(name: String, conclusion: Judg, premises: List[Judg]): Rule =
    rule(Rule(name, conclusion, premises))
  def rule(name: String, conclusion: Judg, premises: Judg*): Rule =
    rule(name, conclusion, premises.toList)
  def rule(rule: Rule): Rule = {
    if (!initialized) {
      if (!rules.contains(rule))
        rules :+= rule
      rule
    }
    else
      throw new RuntimeException(s"Cannot register rule $rule because language was already initialized")
  }

  private var initialized = false
  lazy val language = {
    initialized = true
    Language("STLC", sorts, syms, rules)
  }

  Syntax
  Statics
}