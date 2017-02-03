//package let
//
//import system.Syntax._
//import stlc.Statics._
//import stlc.Syntax._
//import system.Transformation
//
//
//object letstar_empty extends Transformation(stlc.language + let) {
//
//  val Bindings = Sort("Bindings")
//  val noBinding = Symbol("noBinding", in = List(), out = Bindings)
//  val consBinding = Symbol("consBinding", in = List(Name, Exp, Bindings), out = Bindings)
//
//  val letstar = Symbol("letstar", in = List(Bindings, Exp), out = Exp, constr = false)
//
//  override val contract: (Rule, Int) =
//    Lemma("Typed-letstar",
//      Judg(Typed, "C"~Ctx, letstar("bs"~Bindings, "body"~Exp), "T"~Typ),
//      // if ----------------
//      Judg(Typed, "C"~Ctx, letstar("bs"~Bindings, "body"~Exp), "T"~Typ),
//      Judg(Typed, "C"~Ctx, letstar("bs"~Bindings, "body"~Exp), "T"~Typ)
//    ) -> 1
//
//  override val rewrites: Seq[Rewrite] = _
//}
