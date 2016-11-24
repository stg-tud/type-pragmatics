import system.Syntax._

package object stlc {
  private var sorts = Set[ISort]()
  def sort(name: String): Sort =
    sort(Sort(name))
  def sort(sort: Sort): Sort =
    if (!initialized) {
      sorts += sort
      sort
    }
    else
      throw new RuntimeException(s"Cannot register sort $sort because language was already initialized")

  private var syms = Set[Symbol]()
  def symbol(name: String, in: List[ISort], out: ISort): Symbol =
    symbol(Symbol(name, in, out))
  def symbol(sym: Symbol): Symbol =
    if (!initialized) {
      syms += sym
      sym
    }
    else
      throw new RuntimeException(s"Cannot register symbol $sym because language was already initialized")

  private var rules = Set[Rule]()
  def rule(name: String, conclusion: Judg, premises: List[Judg]): Rule =
    rule(Rule(name, conclusion, premises))
  def rule(name: String, conclusion: Judg, premises: Judg*): Rule =
    rule(name, conclusion, premises.toList)
  def rule(r: Rule): Rule = {
    if (!initialized) {
      rules += r
      r
    }
    else
      throw new RuntimeException(s"Cannot register rule $r because language was already initialized")
  }


  private var initialized = false
  lazy val language = {
    initialized = true
    system.Verification.Language(sorts, syms, rules)
  }
}