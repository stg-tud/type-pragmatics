package stlc.cps_onepass

import stlc.Statics._
import stlc.Syntax._
import kont._
import system.Names._
import system.Syntax._
import system.Transformation

object Run_subst extends scala.App {
  println("************ Is subst complete? " + subst.isComplete + " ************\n\n\n\n\n")
  println("************ Is subst contract compliant? " + subst.isContractCompliant + " ************\n\n\n\n\n")
  println("************ Is subst sound? " + subst.isSound + " ************\n\n\n\n\n")
  println("************ Is subst ok? " + subst.isOk + " ************\n\n\n\n\n")
}

object subst extends Transformation(stlc.language + tcps + ccps) {

  override val soundnessTimeout: Int = 90
  override val contractComplianceTimeout: Int = 90

  // CPS expression transformation ecps
  val subst = Symbol("subst", in = List(Exp, Name, Exp, Ctx, Typ, Typ), out = Exp, constr = false)

  override val contract =
    Lemma("T-subst",
      Judg(Typed, "C"~Ctx, subst("e"~Exp, "old"~Name, "new"~Exp, "C"~Ctx, "T"~Typ, "U"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, bind("C"~Ctx, "old"~Name, "U"~Typ), "e"~Exp, "T"~Typ),
      Judg(Typed, "C"~Ctx, "new"~Exp, "U"~Typ)
    ) -> 1


  val subst_ref_equ = Rewrite(
    subst(
      ref("x"~Name),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    "new"~Exp,
    where = Seq(
      Judg(equ(Name), "x"~Name, "old"~Name)
    )
  )

  val subst_ref_neq = Rewrite(
    subst(
      ref("x"~Name),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    ref("x"~Name),
    where = Seq(
      Judg(neq(Name), "x"~Name, "old"~Name)
    )
  )

  val subst_num = Rewrite(
    subst(
      num("n"~Num),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    num("n"~Num)
  )

  val subst_add = Rewrite(
    subst(
      add("e1"~Exp, "e2"~Exp),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    add(
      subst(
        "e1"~Exp,
        "old"~Name,
        "new"~Exp,
        "C"~Ctx,
        "T"~Typ,
        "U"~Typ),
      subst(
        "e2"~Exp,
        "old"~Name,
        "new"~Exp,
        "C"~Ctx,
        "T"~Typ,
        "U"~Typ)
    )
  )

  val subst_lam_equ = Rewrite(
    subst(
      lam("x"~Name, Var("T1", Typ), "e1"~Exp),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    lam("x"~Name, Var("T1", Typ), "e1"~Exp),
    where = Seq(
      Judg(equ(Name), "x"~Name, "old"~Name)
    )
  )

  val subst_lam_neq_nocapture = Rewrite(
    subst(
      lam("x"~Name, Var("T1", Typ), "e1"~Exp),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    lam("x"~Name, Var("T1", Typ),
      subst(
        "e1"~Exp,
        "old"~Name,
        "new"~Exp,
        bind("C"~Ctx, Var("x", Name), Var("T1", Typ)),
        "T2"~Typ,
        "U"~Typ
      )
    ),
    where = Seq(
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), "T"~Typ),
      Judg(neq(Name), "x"~Name, "old"~Name),
      Judg(notin(Ctx), "x"~Name, "C"~Ctx)
    )
  )

  val subst_lam_neq_capture = Rewrite(
    subst(
      lam("x"~Name, Var("T1", Typ), "e1"~Exp),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    lam("xfresh"~Name, Var("T1", Typ),
      subst(
        subst(
          "e1"~Exp,
          "x"~Name,
          ref("xfresh"~Name),
          bind(bind("C"~Ctx, "old"~Name, "U"~Typ), "xfresh"~Name, "T1"~Typ),
          "T2"~Typ,
          "T1"~Typ
        ),
        "old"~Name,
        "new"~Exp,
        bind("C"~Ctx, Var("xfresh", Name), Var("T1", Typ)),
        "T2"~Typ,
        "U"~Typ
      )
    ),
    where = Seq(
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), "T"~Typ),
      Judg(neq(Name), "x"~Name, "old"~Name),
      Judg(NOT, notin(Ctx)("x"~Name, "C"~Ctx)),
      Judg(equ(Name), "xfresh"~Name, fresh(Ctx)(bind("C"~Ctx, "x"~Name, "T1"~Typ)))
    )
  )

  val subst_app = Rewrite(
    subst(
      app("e1"~Exp, "e2"~Exp),
      "old"~Name,
      "new"~Exp,
      "C"~Ctx,
      "T"~Typ,
      "U"~Typ
    ),
    // ~>
    app(
      subst(
        "e1"~Exp,
        "old"~Name,
        "new"~Exp,
        "C"~Ctx,
        Arr("T1"~Typ, "T"~Typ),
        "U"~Typ
      ),
      subst(
        "e2"~Exp,
        "old"~Name,
        "new"~Exp,
        "C"~Ctx,
        "T1"~Typ,
        "U"~Typ
      )
    ),
    where = Seq(
      Judg(Typed, bind("C"~Ctx, "old"~Name, "U"~Typ), "e1"~Exp, Arr(Var("T1", Typ), "T"~Typ))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(
    subst_ref_equ,
    subst_ref_neq,
    subst_num,
    subst_add,
    subst_lam_equ,
    subst_lam_neq_nocapture,
    subst_lam_neq_capture,
    subst_app
  )

  checkSyntax()
}

