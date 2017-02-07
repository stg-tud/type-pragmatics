package delta

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

import scala.collection.immutable.ListMap

object edelta extends Transformation(stlc.language + delta_ext + tdelta + cdelta + copyV)  {

  override val wellformednessTimeout: Int = 120

  val edelta = Symbol("edelta", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract: (Rule, Int) =
    Lemma("T-edelta",
      Judg(Typed, cdelta("C"~Ctx), edelta("e"~Exp, "C"~Ctx, "T"~Typ), tdelta("T"~Typ)),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
      Judg(TOk, "T"~Typ),
      Judg(CtxOk, "C"~Ctx)
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
        edelta("e1"~Exp, "C"~Ctx, Arr("T1"~Typ, "T"~Typ)),
        copyV("e2"~Exp, "C"~Ctx, "T1"~Typ)),
      edelta("e2"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(TOk, Var("T1", Typ)),
      Judg(Typed, Var("C", Ctx), Var("e1", Exp), Arr(Var("T1", Typ), Var("T", Typ)))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(edelta_ref, edelta_num, edelta_add, edelta_lam, edelta_app)

  checkSyntax()
}
