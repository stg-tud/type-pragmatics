package stlc.cps_onepass

import system.Syntax._
import system.{Syntax, Transformation}

import stlc.Syntax._
import stlc.Statics._
import kont._

object Run_applyKont extends scala.App {
  println("************ Is applyKont complete? " + applyKont.isComplete + " ************\n\n\n\n\n")
  println("************ Is applyKont contract compliant? " + applyKont.isContractCompliant + " ************\n\n\n\n\n")
  println("************ Is applyKont sound? " + applyKont.isSound + " ************\n\n\n\n\n")
  println("************ Is applyKont ok? " + applyKont.isOk + " ************\n\n\n\n\n")
}

object applyKont extends Transformation(stlc.language + kont.ext + subst) {
  import kont._

  val applyKont = Symbol("applyKont", in = List(Exp, Kont, Ctx, Typ), out = Exp, constr = false)

  override val contract: (Syntax.Rule, Int) =
    Lemma("T-applyKont",
      Judg(Typed, "C"~Ctx, applyKont("e"~Exp, "k"~Kont, "C"~Ctx, "T"~Typ), "R"~Typ),
      // if -------------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
      Judg(TypedKont, "C"~Ctx, "k"~Kont, Arr("T"~Typ, "R"~Typ))
    ) -> 1


  val applyKont_kont = Rewrite(
    applyKont(
      "e"~Exp,
      kont("kx"~Name, "kT"~Typ, "ke"~Exp),
      "C"~Ctx,
      "T"~Typ
    ),
    // ~>
    subst(
      "ke"~Exp,
      "kx"~Name,
      "e"~Exp,
      "C"~Ctx,
      "kT"~Typ,
      "T"~Typ
    )
  )

  override val rewrites: Seq[Syntax.Rewrite] = Seq(applyKont_kont)
}