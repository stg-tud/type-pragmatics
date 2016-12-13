package stlc

import system.Syntax._
import Syntax._
import system.Names.notin

object Statics {

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
    Judg(neq(Name), Var("x", Name), Var("y", Name)),
    Judg(Lookup, Var("x", Name), Var("T", Typ), Var("C", Ctx))
  )

  val Notin_Empty = rule("Notin-empty",
    Judg(notin(Ctx), Var("x", Name), empty())
    // if ----------------
  )
  val Notin_Bind = rule("Notin-bind",
    Judg(notin(Ctx), Var("x", Name), bind(Var("C", Ctx), Var("y", Name), Var("T", Typ))),
    // if ----------------
    Judg(neq(Name), Var("x", Name), Var("y", Name)),
    Judg(notin(Ctx), Var("x", Name), Var("C", Ctx))
  )

  val Lookup_Notin = rule(Lemma("Lookup-Notin",
    Judg(neq(Ctx), Var("C1", Ctx), Var("C2", Ctx)),
    // if ----------------
    Judg(Lookup, Var("x", Name), Var("T", Typ), Var("C1", Ctx)),
    Judg(notin(Ctx), Var("x", Name), Var("C2", Ctx))
  ))

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

  val CtxOk = symbol("CtxOk", in = List(Ctx), out = Prop)
  val CtxOk_Nat = rule("CtxOk-empty",
    Judg(CtxOk, App(empty))
    // if ----------------
  )
  val CtxOk_bind = rule("CtxOk-Arr",
    Judg(CtxOk, bind(Var("C", Ctx), Var("x", Name), Var("T", Typ))),
    // if ----------------
    Judg(TOk, Var("T", Typ)),
    Judg(CtxOk, Var("C", Ctx))
  )

  val Typed = symbol("Typed", in = List(Ctx, Exp, Typ), out = Prop)
  val Typed_ref = rule("Typed-ref",
    Judg(Typed, Var("C", Ctx), ref(Var("x", Name)), Var("T", Typ)),
    // if ----------------
    Judg(Lookup, Var("x", Name), Var("T", Typ), Var("C", Ctx))
  )
  val Typed_num = rule("Typed-num",
    Judg(Typed, Var("C", Ctx), num(Var("n", Num)), Nat())
    // if ----------------
  )
  val Typed_add = rule("Typed-add",
    Judg(Typed, Var("C", Ctx), add(Var("e1", Exp), Var("e2", Exp)), Nat()),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e1", Exp), Nat()),
    Judg(Typed, Var("C", Ctx), Var("e2", Exp), Nat())
  )
  val Typed_lam = rule("Typed-lam",
    Judg(Typed,
      Var("C", Ctx),
      lam(Var("x", Name), Var("T1", Typ), Var("e", Exp)),
      Arr(Var("T1", Typ), Var("T2", Typ))),
    // if ----------------
    Judg(Typed,
      bind(Var("C", Ctx), Var("x", Name), Var("T1", Typ)),
      Var("e", Exp),
      Var("T2", Typ))
  )
  val Typed_app = rule("Typed-app",
    Judg(Typed, Var("C", Ctx), app(Var("e1", Exp), Var("e2", Exp)), Var("T2", Typ)),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e1", Exp), Arr(Var("T1", Typ), Var("T2", Typ))),
    Judg(Typed, Var("C", Ctx), Var("e2", Exp), Var("T1", Typ))
  )

  val Typed_uniqueness = rule(Lemma("Typed-uniqueness",
    Judg(equ(Typ), Var("T1", Typ), Var("T2", Typ)),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T1", Typ)),
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T2", Typ))
  ))
  val Typed_weakening = rule(Lemma("Typed-weakening",
    Judg(Typed, bind(Var("C", Ctx), Var("x", Name), Var("Tx", Typ)), Var("e", Exp), Var("T", Typ)),
    // if ----------------
    Judg(notin(Ctx), Var("x", Name), Var("C", Ctx)),
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T", Typ))
  ))
  val Typed_exchange = rule(Lemma("Typed-exchange",
    Judg(Typed,
      bind(bind(
        Var("C", Ctx),
        Var("x", Name), Var("Tx", Typ)),
        Var("y", Name), Var("Ty", Typ)),
      Var("e", Exp),
      Var("T", Typ)),
    // if ----------------
    Judg(neq(Name), Var("x", Name), Var("y", Name)),
    Judg(Typed,
      bind(bind(
        Var("C", Ctx),
        Var("y", Name), Var("Ty", Typ)),
        Var("x", Name), Var("Tx", Typ)),
      Var("e", Exp),
      Var("T", Typ))
  ))
  val Typed_contraction = rule(Lemma("Typed-contraction",
    Judg(Typed,
      bind(
        Var("C", Ctx),
        Var("x", Name), Var("Tx", Typ)),
      Var("e", Exp),
      Var("T", Typ)),
    // if ----------------
    Judg(Typed,
      bind(bind(
        Var("C", Ctx),
        Var("x", Name), Var("Tx", Typ)),
        Var("x", Name), Var("Tx", Typ)),
      Var("e", Exp),
      Var("T", Typ))
  ))
  val Typed_TOk = rule(Lemma("Typed-TOk",
    Judg(TOk, Var("T", Typ)),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T", Typ))
  ))
}