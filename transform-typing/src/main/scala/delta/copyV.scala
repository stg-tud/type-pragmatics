package delta

import system.Transformation
import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation
import system.Names.notin

import scala.collection.immutable.ListMap

object copyV extends Transformation(stlc.language + ext + tdelta + cdelta) {
  val copyV = Symbol("copyV", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract =
    Lemma("Typed-copyV",
      Judg(Typed, cdelta("C"~Ctx), copyV("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
      Judg(CtxOk, "C"~Ctx)
    ) -> 1

  override val lemmas: ListMap[Rule, Int] = ListMap(
    Lemma("notin-d-copyV",
      Judg(notin(Exp), d("x"~Name), copyV("e"~Exp, "C"~Ctx, "T"~Typ)),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
      Judg(CtxOk, "C"~Ctx)
    ) -> 1
  )

  val copyV_ref = Rewrite(
    copyV(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref(v("x"~Name))
  )

  val copyV_num = Rewrite(
    copyV(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )

  val copyV_add = Rewrite(
    copyV(add("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    add(copyV("e1"~Exp, "C"~Ctx, "T"~Typ), copyV("e2"~Exp, "C"~Ctx, "T"~Typ))
  )

  val copyV_lam = Rewrite(
    copyV(lam("x"~Name, "T1"~Typ, "e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    lam(v("x"~Name), "T1"~Typ,
      copyV("e"~Exp, bind("C"~Ctx, "x"~Name, "T1"~Typ), "T2"~Typ)
    ),
    where = Seq(
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), Var("T", Typ))
    )
  )

  val copyV_app = Rewrite(
    copyV(app("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    app(
      copyV("e1"~Exp, "C"~Ctx, Arr("T1"~Typ, "T"~Typ)),
      copyV("e2"~Exp, "C"~Ctx, "T1"~Typ)),
    where = Seq(
      Judg(Typed, Var("C", Ctx), Var("e1", Exp), Arr(Var("T1", Typ), Var("T", Typ)))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(copyV_ref, copyV_num, copyV_add, copyV_lam, copyV_app)

  checkSyntax()
}
