package stlc

import system.Syntax._
import Syntax._

object Statics {

  val TOk = Symbol("TOk", in = List(Typ), out = Prop)
  val TOk_Nat = Rule("TOk-Nat",
    Judg(TOk, App(Nat))
    // if ----------------
  )
  val TOk_Arr = Rule("TOk-Arr",
    Judg(TOk, App(Arr, Var("t1", Typ), Var("t2", Typ))),
    // if ----------------
    Judg(TOk, Var("t1", Typ)),
    Judg(TOk, Var("t2", Typ))
  )


  val Lookup = Symbol("Lookup", in = List(Nam, Typ, Ctx), out = Prop)
  // TODO rules

}