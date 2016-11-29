package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Transformation

object ccps extends Transformation(stlc.language + tcps) {

  // CPS context transformation ccps
  val ccps = Symbol("ccps", in = List(Ctx, Typ), out = Ctx, constr = false)

  private val omega = Var("omega", Typ)

  override val contract = Rule("Lookup-ccps",
    Judg(Lookup,
      Var("x", Name),
      App(tcps, Var("T", Typ), omega),
      App(ccps, Var("C", Ctx), omega)),
    // if ----------------
    Judg(Lookup,
      Var("x", Name),
      Var("T", Typ),
      Var("C", Ctx))
  )

  override val contractPos: Int = 2


  val ccps_empty = Rewrite(
    App(ccps, App(empty), omega),
    // ~>
    App(empty)
  )

  val ccps_bind = Rewrite(
    App(ccps,
      App(bind,
        Var("C", Ctx),
        Var("x", Name),
        Var("T", Typ)),
      omega),
    // ~>
    App(bind,
      App(ccps,
        Var("C", Ctx),
        omega),
      Var("x", Name),
      App(tcps,
        Var("T", Typ),
        omega
      )
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(ccps_empty, ccps_bind)
}
