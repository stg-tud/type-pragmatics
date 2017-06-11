package stlc.delta

import system.Transformation
import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation
import system.Names.notin

import scala.collection.immutable.ListMap

object fit extends Transformation(stlc.language + delta_ext + tdelta + cdelta) {
  val fit = Symbol("fit", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract =
    Lemma("Typed-fit",
      Judg(Typed, cdelta("C"~Ctx), fit("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
      Judg(CtxOk, "C"~Ctx)
    ) -> 1

  val fit_ref = Rewrite(
    fit(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref(v("x"~Name))
  )

  val fit_num = Rewrite(
    fit(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )

  val fit_add = Rewrite(
    fit(add("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    add(fit("e1"~Exp, "C"~Ctx, "T"~Typ), fit("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  val fit_lam = Rewrite(
    fit(lam("x"~Name, "T1"~Typ, "e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    lam(v("x"~Name), "T1"~Typ,
      fit("e"~Exp, bind("C"~Ctx, "x"~Name, "T1"~Typ), "T2"~Typ)
    ),
    where = Seq(
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), Var("T", Typ))
    )
  )

  val fit_app = Rewrite(
    fit(app("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      fit("e1"~Exp, "C"~Ctx, Arr("T1"~Typ, "T"~Typ)),
      fit("e2"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(Typed, Var("C", Ctx), Var("e1", Exp), Arr(Var("T1", Typ), Var("T", Typ)))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(fit_ref, fit_num, fit_add, fit_lam, fit_app)

  checkSyntax()
}
