package stlc

import system.Syntax._
import Syntax._
import system.Names._

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


  val Lookup = Symbol("Lookup", in = List(Name, Typ, Ctx), out = Prop)
  val Lookup_Found = Rule("Lookup-Found",
    Judg(Lookup,
      Var("x", Name),
      Var("T", Typ),
      App(bind, Var("C", Ctx), Var("x", Name), Var("T", Typ)))
    // if ----------------
  )
  val Lookup_Next = Rule("Lookup-Next",
    Judg(Lookup,
      Var("x", Name),
      Var("T", Typ),
      App(bind, Var("C", Ctx), Var("y", Name), Var("S", Typ))),
    // if ----------------
    Judg(NeqNam, Var("x", Name), Var("y", Name)),
    Judg(Lookup, Var("x", Name), Var("T", Typ), Var("C", Ctx))
  )

}