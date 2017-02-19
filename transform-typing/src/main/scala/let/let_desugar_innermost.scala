package let

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

object let_desugar_innermost extends Transformation(stlc.language + let_ext) {

  override val contractComplianceTimeout: Int = 120

  // desugaring transformation
  val letdesugar = Symbol("letdesugar", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract: (Rule, Int) =
    Lemma("Typed-let-desugar",
      Judg(Typed, "C"~Ctx, letdesugar("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
    ) -> 1

  val letdesugar_var = Rewrite(
    letdesugar(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref("x"~Name)
  )

  val letdesugar_num = Rewrite(
    letdesugar(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )

  val letdesugar_add = Rewrite(
    letdesugar(add("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    add(
      letdesugar("e1"~Exp, "C"~Ctx, "T"~Typ),
      letdesugar("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  val letdesugar_lam = Rewrite(
    letdesugar(lam("x"~Name, "T1"~Typ, "e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    lam("x"~Name, "T1"~Typ,
      letdesugar("e"~Exp, bind("C"~Ctx, "x"~Name, "T1"~Typ), "T2"~Typ)),
    where = Seq(
      Judg(equ(Typ), "T"~Typ, Arr("T1"~Typ, "T2"~Typ))
    )
  )

  val letdesugar_app = Rewrite(
    letdesugar(app("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      letdesugar("e1"~Exp, "C"~Ctx, Arr("T1"~Typ, "T"~Typ)),
      letdesugar("e2"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, Arr("T1"~Typ, "T"~Typ))
    )
  )

  val letdesugar_let = Rewrite(
    letdesugar(let("x"~Name, "e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    letdesugar(
      app(lam("x"~Name, "T1"~Typ, "e2"~Exp), "e1"~Exp),
      "C"~Ctx,
      "T"~Typ),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, "T1"~Typ)
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(
    letdesugar_var, letdesugar_num, letdesugar_add, letdesugar_lam, letdesugar_app, letdesugar_let
  )

  checkSyntax()
}
