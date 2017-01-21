package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Transformation
import system.Names.notin

import scala.collection.immutable.ListMap

object ccps extends Transformation(stlc.language + tcps) {

  // CPS context transformation ccps
  val ccps = Symbol("ccps", in = List(Ctx, Typ), out = Ctx, constr = false)

  private val omega = Var("omega", Typ)

  override val contract: (Rule, Int) =
    Lemma("CtxOk-ccps",
      Judg(CtxOk, ccps(Var("C", Ctx), omega)),
      // if ----------------
      Judg(CtxOk, Var("C", Ctx)),
      Judg(TOk, omega)
    ) -> 0

  override val lemmas = ListMap(
    Lemma("Lookup-ccps",
      Judg(Lookup,
        Var("x", Name),
        tcps(Var("T", Typ), omega),
        ccps(Var("C", Ctx), omega)),
      // if ----------------
      Judg(Lookup,
        Var("x", Name),
        Var("T", Typ),
        Var("C", Ctx)),
      Judg(CtxOk, Var("C", Ctx)),
      Judg(TOk, Var("T", Typ)),
      Judg(TOk, omega)
    ) -> 2,

    Lemma("Notin-ccps",
      Judg(notin(Ctx), Var("x", Name), ccps(Var("C", Ctx), omega)),
      // if ----------------
      Judg(notin(Ctx), Var("x", Name), Var("C", Ctx)),
      Judg(CtxOk, Var("C", Ctx)),
      Judg(TOk, omega)
    ) -> 1
  )

  val ccps_empty = Rewrite(
    App(ccps, App(empty), omega),
    // ~>
    App(empty)
  )

  val ccps_bind = Rewrite(
    ccps(
      bind(Var("C", Ctx), Var("y", Name), Var("T", Typ)),
      omega),
    // ~>
    bind(
      ccps(Var("C", Ctx), omega),
      Var("y", Name),
      tcps(Var("T", Typ), omega)
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(ccps_empty, ccps_bind)

  checkSyntax()
}
