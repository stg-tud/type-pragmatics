package stlc

import system.Syntax._
import system.Names._

object Syntax {
  // sorts
  val Num = sort("Num")
  val Typ = sort("Typ")
  val Ctx = sort("Ctx")
  val Exp = sort("Exp")

  // types
  val Nat = symbol("Nat", in = List(), out = Typ)
  val Arr = symbol("Arr", in = List(Typ, Typ), out = Typ)

  // contexts
  val ∅ = symbol("∅", in = List(), out = Ctx)
  val bind = symbol("bind", in = List(Ctx, Name, Typ), out = Ctx)

  // expressions
  val ref = symbol("ref", in = List(Name), out = Exp)
  val num = symbol("num", in = List(Num), out = Exp)
  val add = symbol("add", in = List(Exp, Exp), out = Exp)
  val lam = symbol("lam", in = List(Name, Typ, Exp), out = Exp)
  val app = symbol("app", in = List(Exp, Exp), out = Exp)
}