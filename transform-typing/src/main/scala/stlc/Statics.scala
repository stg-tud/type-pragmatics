package stlc

import system.Syntax._
import Syntax._
import system.Names._

object Statics {

  val TOk = symbol("TOk", in = List(Typ), out = Prop)
  val TOk_Nat = rule("TOk-Nat",
    Judg(TOk, App(Nat))
    // if ----------------
  )
  val TOk_Arr = rule("TOk-Arr",
    Judg(TOk, App(Arr, Var("t1", Typ), Var("t2", Typ))),
    // if ----------------
    Judg(TOk, Var("t1", Typ)),
    Judg(TOk, Var("t2", Typ))
  )


  val Lookup = symbol("Lookup", in = List(Name, Typ, Ctx), out = Prop)
  val Lookup_Found = rule("Lookup-Found",
    Judg(Lookup,
      Var("x", Name),
      Var("T", Typ),
      App(bind, Var("C", Ctx), Var("x", Name), Var("T", Typ)))
    // if ----------------
  )
  val Lookup_Next = rule("Lookup-Next",
    Judg(Lookup,
      Var("x", Name),
      Var("T", Typ),
      App(bind, Var("C", Ctx), Var("y", Name), Var("S", Typ))),
    // if ----------------
    Judg(NameNeq, Var("x", Name), Var("y", Name)),
    Judg(Lookup, Var("x", Name), Var("T", Typ), Var("C", Ctx))
  )

}