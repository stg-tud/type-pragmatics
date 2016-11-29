package system

import system.Syntax._

object Names {

  val NameNeq = Symbol("NameEq", in = List(Name, Name), out = Prop)

  def fresh(s: Sort) = Symbol(s"fresh-$s", in = List(s), out = Name)
  def notin(s: Sort) = Symbol(s"notin-$s", in = List(Name, s), out = Prop)

}