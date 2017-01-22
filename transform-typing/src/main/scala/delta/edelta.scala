package delta

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation
import cdelta.{d,v}

object edelta extends Transformation(stlc.language + tdelta + cdelta)  {

  val edelta = Symbol("edelta", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  val zero = Symbol("zero", in = List(), out = Num, constr = false)
  override val extraSymbols: Seq[Symbol] = Seq(zero)

  override val contract: (Rule, Int) =
    Rule("T-edelta",
      Judg(Typed, cdelta("C"~Ctx), edelta("e"~Exp, "C"~Ctx, "T"~Typ), tdelta("T"~Typ)),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
    ) -> 1


  val edelta_ref = Rewrite(
    edelta(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref(d("x"~Name))
  )

  val edelta_num = Rewrite(
    edelta(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num(zero())
  )

  val edelta_add = Rewrite(
    edelta(add("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    add(edelta("e1"~Exp, "C"~Ctx, "T"~Typ), edelta("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  val edelta_lam = Rewrite(
    edelta(lam("x"~Name, "T1"~Typ, "e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    lam(v("x"~Name), "T1"~Typ,
      lam(d("x"~Name), tdelta("T1"~Typ),
        edelta("e"~Exp, bind("C"~Ctx, "x"~Name, "T1"~Typ), "T2"~Typ)
      )
    ),
    where = Seq(
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), Var("T", Typ))
    )
  )

  val edelta_app = Rewrite(
    edelta(app("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      app(
        edelta("e1"~Exp, "C"~Ctx, "T"~Typ),
        "e2"~Exp),
      edelta("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  override val rewrites: Seq[Rewrite] = Seq(edelta_ref, edelta_num, edelta_add, edelta_lam, edelta_app)
}
