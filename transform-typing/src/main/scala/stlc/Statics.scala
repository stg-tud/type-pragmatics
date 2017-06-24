package stlc

import system.Syntax._
import Syntax._
import system.Names
import system.Names.notin

object Statics {

  val Lookup = symbol("Lookup", in = List(Name, Typ, Ctx), out = Prop)
  val Lookup_Found = rule("Lookup-Found",
    Judg(Lookup,
      "x"~Name,
      "T"~Typ,
      App(bind, "C"~Ctx, "x"~Name, "T"~Typ))
    // if ----------------
  )
  val Lookup_Next = rule("Lookup-Next",
    Judg(Lookup,
      "x"~Name,
      "T"~Typ,
      App(bind, "C"~Ctx, "y"~Name, "S"~Typ)),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx)
  )
  val Lookup_Bind_Inv = rule(Lemma("Lookup-Bind-Inv",
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, bind("C"~Ctx, "y"~Name, "Ty"~Typ)),
    Judg(neq(Name), "x"~Name, "y"~Name)
  ))

  val Notin_Empty = rule("Notin-empty",
    Judg(notin(Ctx), "x"~Name, empty())
    // if ----------------
  )
  val Notin_Bind = rule("Notin-bind",
    Judg(notin(Ctx), "x"~Name, bind("C"~Ctx, "y"~Name, "T"~Typ)),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(notin(Ctx), "x"~Name, "C"~Ctx)
  )

  val Lookup_Notin = rule(Lemma("Lookup-Notin",
    Judg(neq(Ctx), "C1"~Ctx, "C2"~Ctx),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, "C1"~Ctx),
    Judg(notin(Ctx), "x"~Name, "C2"~Ctx)
  ))

  val TOk = symbol("TOk", in = List(Typ), out = Prop)
  val TOk_Nat = rule("TOk-Nat",
    Judg(TOk, App(Nat))
    // if ----------------
  )
  val TOk_Arr = rule("TOk-Arr",
    Judg(TOk, App(Arr, "t1"~Typ, "t2"~Typ)),
    // if ----------------
    Judg(TOk, "t1"~Typ),
    Judg(TOk, "t2"~Typ)
  )

  val CtxOk = symbol("CtxOk", in = List(Ctx), out = Prop)
  val CtxOk_empty = rule("CtxOk-empty",
    Judg(CtxOk, App(empty))
    // if ----------------
  )
  val CtxOk_bind = rule("CtxOk-Arr",
    Judg(CtxOk, bind("C"~Ctx, "x"~Name, "T"~Typ)),
    // if ----------------
    Judg(TOk, "T"~Typ),
    Judg(CtxOk, "C"~Ctx)
  )

  val Typed = symbol("Typed", in = List(Ctx, Exp, Typ), out = Prop)
  val Typed_ref = rule("Typed-ref",
    Judg(Typed, "C"~Ctx, ref("x"~Name), "T"~Typ),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx)
  )
  val Typed_num = rule("Typed-num",
    Judg(Typed, "C"~Ctx, num("n"~Num), Nat())
    // if ----------------
  )
  val Typed_add = rule("Typed-add",
    Judg(Typed, "C"~Ctx, add("e1"~Exp, "e2"~Exp), Nat()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Nat()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Nat())
  )
  val Typed_lam = rule("Typed-lam",
    Judg(Typed,
      "C"~Ctx,
      lam("x"~Name, "T1"~Typ, "e"~Exp),
      Arr("T1"~Typ, "T2"~Typ)),
    // if ----------------
    Judg(Typed,
      bind("C"~Ctx, "x"~Name, "T1"~Typ),
      "e"~Exp,
      "T2"~Typ),
    Judg(TOk, "T1"~Typ)
  )
  val Typed_app = rule("Typed-app",
    Judg(Typed, "C"~Ctx, app("e1"~Exp, "e2"~Exp), "T2"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Arr("T1"~Typ, "T2"~Typ)),
    Judg(Typed, "C"~Ctx, "e2"~Exp, "T1"~Typ)
  )

  val Typed_uniqueness = rule(Lemma("Typed-uniqueness",
    Judg(equ(Typ), "T1"~Typ, "T2"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T1"~Typ),
    Judg(Typed, "C"~Ctx, "e"~Exp, "T2"~Typ)
  ))
  val Typed_TOk = rule(Lemma("Typed-TOk",
    Judg(TOk, "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  ))
  val Typed_weakening = rule(Lemma("Typed-weakening",
    Judg(Typed, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "e"~Exp, "T"~Typ),
    // if ----------------
    Judg(notin(Ctx), "x"~Name, "C"~Ctx),
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  ))
  val Typed_exchange = rule(Lemma("Typed-exchange",
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "y"~Name, "Ty"~Typ),
      "e"~Exp,
      "T"~Typ),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "y"~Name, "Ty"~Typ),
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ)
  ))
  val Typed_contraction = rule(Lemma("Typed-contraction",
    Judg(Typed,
      bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ),
    // if ----------------
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ)
  ))
  val Typed_strengthening = rule(Lemma("Typed-strengthening",
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
    // if ----------------
    Judg(notin(Exp), "x"~Name, "e"~Exp),
    Judg(Typed, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "e"~Exp, "T"~Typ)
  ))
}