package stlc

import system.Syntax._

object Syntax {
  // sorts
  val Nam = Sort("Nam")
  val Num = Sort("Num")
  val Typ = Sort("Typ")
  val Ctx = Sort("Ctx")
  val Exp = Sort("Exp")

  // types
  val Nat = Symbol("Nat", in = List(), out = Typ)
  val Arr = Symbol("Arr", in = List(Typ, Typ), out = Typ)

  // contexts
  val ∅ = Symbol("∅", in = List(), out = Ctx)
  val bind = Symbol("bind", in = List(Ctx, Nam, Typ), out = Ctx)

  // expressions
  val ref = Symbol("ref", in = List(Nam), out = Exp)
  val num = Symbol("num", in = List(Num), out = Exp)
  val add = Symbol("add", in = List(Exp, Exp), out = Exp)
  val lam = Symbol("lam", in = List(Nam, Typ, Exp), out = Exp)
  val app = Symbol("app", in = List(Exp, Exp), out = Exp)
}