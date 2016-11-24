package system

import system.Syntax._

object Names {
  case object Name extends ISort

  val NameNeq = Symbol("NameEq", in = List(Name, Name), out = Prop)

}