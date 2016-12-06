package system

import system.Syntax._

object Names {

  def fresh(s: ISort) = Symbol(s"fresh$s", in = List(s), out = Name, false)
  def notin(s: ISort) = Symbol(s"notin$s", in = List(Name, s), out = Prop)

  def freshNotin(s: ISort) = {
    val freshSym = Names.fresh(s)
    val notinSym = Names.notin(s)
    Rule(s"$freshSym-$notinSym",
      Judg(notinSym, freshSym(Var("x", s)), Var("x", s))
    )
  }
}