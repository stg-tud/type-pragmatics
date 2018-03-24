package stlc.cps_onepass

import system.{LanguageExtension, Transformation}
import stlc.Syntax._
import stlc.Statics.Typed
import system.Syntax._

object kont {
  val Kont = Sort("Kont")
  val kont = Symbol("kont", List(Name, Typ, Exp), Kont)

  val TypedKont = Symbol("TypedKont", in = List(Ctx,Kont,Typ), out = Prop)

  val TypedKont_kont = Rule("TypedKont-kont",
    Judg(TypedKont, "C"~Ctx, kont("x"~Name, "T"~Typ, "e"~Exp), Arr("T"~Typ, "R"~Typ)),
    // if
    Judg(Typed, "C"~Ctx, lam("x"~Name, "T"~Typ, "e"~Exp), Arr("T"~Typ, "R"~Typ))
  )

  val ext = LanguageExtension("kont", Seq(Kont), Seq(kont, TypedKont), Seq(TypedKont_kont))
}
