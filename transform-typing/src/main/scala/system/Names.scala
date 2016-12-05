package system

import system.Syntax._

object Names {

  def fresh(s: Sort) = Symbol(s"fresh$s", in = List(s), out = Name, false)
  def notin(s: Sort) = Symbol(s"notin$s", in = List(Name, s), out = Prop)

}