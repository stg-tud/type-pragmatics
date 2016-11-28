package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Names._

import cps.Types._

object Contexts {
  // for
  stlc.language

  private val omega = Var("omega", Typ)

  // CPS type transformation tcps
  val ccps = Symbol("ccps", in = List(Ctx, Typ), out = Ctx, constr = false)

  val ccps_contract = Rule("Lookup-ccps",
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

  val ccps_transform = Transform(
    ccps_contract,
    2,
    ccps_empty,
    ccps_bind
  )

  val soundnessObligations = system.Soundness.transSoundness(ccps_transform, stlc.language)
}
