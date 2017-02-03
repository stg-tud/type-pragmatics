import stlc.Statics._
import stlc.Syntax._
import system.LanguageExtension
import system.Syntax._

package object let {
  // let syntax
  val let = Symbol("let", in = List(Name, Exp, Exp), out = Exp, constr = true)

  val Typed_let = Rule("Typed-let",
    Judg(Typed, "C"~Ctx, let("x"~Name, "e1"~Exp, "e2"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, "T1"~Typ),
    Judg(Typed, bind("C"~Ctx, "x"~Name, "T1"~Typ), "e"~Exp, "T"~Typ)
  )

  object letext extends LanguageExtension("let-ext", Seq(), Seq(let), Seq(Typed_let))
}
