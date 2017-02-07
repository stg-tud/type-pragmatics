package let

import system.Syntax._
import stlc.Statics._
import stlc.Syntax._
import system.Transformation


object letstar_desugar extends Transformation(stlc.language + letstar_ext) {

  // desugaring transformation
  val letstardesugar = Symbol("letstardesugar", in = List(Exp, Ctx, Typ), out = Exp, constr = false)
  
  override val contract: (Rule, Int) =
    Lemma("Typed-letstar-desugar",
      Judg(Typed, "C"~Ctx, letstardesugar("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
    ) -> 1

  val letstardesugar_var = Rewrite(
    letstardesugar(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref("x"~Name)
  )

  val letstardesugar_num = Rewrite(
    letstardesugar(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )

  val letstardesugar_add = Rewrite(
    letstardesugar(add("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    add(
      letstardesugar("e1"~Exp, "C"~Ctx, "T"~Typ),
      letstardesugar("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  val letstardesugar_lam = Rewrite(
    letstardesugar(lam("x"~Name, "T1"~Typ, "e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    lam("x"~Name, "T1"~Typ,
      letstardesugar("e"~Exp, bind("C"~Ctx, "x"~Name, "T1"~Typ), "T2"~Typ)),
    where = Seq(
      Judg(equ(Typ), "T"~Typ, Arr("T1"~Typ, "T2"~Typ))
    )
  )

  val letstardesugar_app = Rewrite(
    letstardesugar(app("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      letstardesugar("e1"~Exp, "C"~Ctx, Arr("T1"~Typ, "T"~Typ)),
      letstardesugar("e2"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, Arr("T1"~Typ, "T"~Typ))
    )
  )

  val letstardesugar_letstar_nil = Rewrite(
    letstardesugar(letstar(nilBinding(), "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    "e2"~Exp
  )
  
  val letstardesugar_letstar_cons = Rewrite(
    letstardesugar(letstar(consBinding("x"~Name, "e1"~Exp, "bs"~Bindings), "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      lam("x"~Name, "T1"~Typ,
        letstardesugar(letstar("bs"~Bindings, "e2"~Exp), bind("C"~Ctx, "x"~Name, "T1"~Typ), "T"~Typ)),
      letstardesugar("e1"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, "T1"~Typ)
    )
  )
  
  override val rewrites: Seq[Rewrite] = Seq(
    letstardesugar_var, letstardesugar_num, letstardesugar_add, letstardesugar_lam, letstardesugar_app,
    letstardesugar_letstar_nil, letstardesugar_letstar_cons
  )

  checkSyntax()
}
