package stlc

import stlc.Statics._
import stlc.Syntax._
import system.LanguageExtension
import system.Syntax._

package object let {
  // let syntax
  val let = Symbol("let", in = List(Name, Exp, Exp), out = Exp)

  val Typed_let = Rule("Typed-let",
    Judg(Typed, "C"~Ctx, let("x"~Name, "e1"~Exp, "e2"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, "T1"~Typ),
    Judg(Typed, bind("C"~Ctx, "x"~Name, "T1"~Typ), "e2"~Exp, "T"~Typ)
  )

  object let_ext extends LanguageExtension("let-ext", Seq(), Seq(let), Seq(Typed_let))


  // letstar syntax
  val Bindings = Sort("Bindings")
  val nilBinding = Symbol("noBinding", in = List(), out = Bindings)
  val consBinding = Symbol("consBinding", in = List(Name, Exp, Bindings), out = Bindings)

  val letstar = Symbol("letstar", in = List(Bindings, Exp), out = Exp)

  val Typed_letstar_nil = Rule("Typed-letstar-nil",
    Judg(Typed, "C"~Ctx, letstar(nilBinding(), "e2"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e2"~Exp, "T"~Typ)
  )

  val Typed_letstar_cons = Rule("Typed-letstar-cons",
    Judg(Typed, "C"~Ctx, letstar(consBinding("x"~Name, "e1"~Exp, "bs"~Bindings), "e2"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, "T1"~Typ),
    Judg(Typed, bind("C"~Ctx, "x"~Name, "T1"~Typ), letstar("bs"~Bindings, "e2"~Exp), "T"~Typ)
  )

  object letstar_ext extends LanguageExtension(
    "letstar-ext",
    Seq(Bindings),
    Seq(nilBinding, consBinding, letstar),
    Seq(Typed_letstar_nil, Typed_letstar_cons)
  )
}
