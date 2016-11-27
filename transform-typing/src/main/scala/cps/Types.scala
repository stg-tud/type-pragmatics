package cps

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._

object Types {
  // for
  stlc.language

  private val omega = Var("omega", Typ)

  // CPS type transformation tcps
  val tcps = Symbol("tcps", in = List(Typ, Typ), out = Typ, constr = false)

  val tcps_contract = Rule("TOk-tcps",
    Judg(TOk, App(tcps, Var("T", Typ), omega)),
    // if ----------------
    Judg(TOk, Var("T", Typ)),
    Judg(TOk, omega)
  )

  val tcps_nat = Rewrite(
    App(tcps, App(Nat), omega),
    // ~>
    App(Nat)
  )

  val tcps_arr = Rewrite(
    App(tcps, App(Arr, Var("T1", Typ), Var("T2", Typ)), omega),
    // ~>
    App(Arr,
      App(tcps, Var("T1", Typ), omega),
      App(Arr,
        App(Arr,
          App(tcps, Var("T2", Typ), omega),
          omega
        ),
        omega
      )
    )
  )

  val tcps_transform = Transform(
    tcps_contract,
    0,
    tcps_nat,
    tcps_arr
  )

  val soundnessObligations = system.Soundness.transSoundness(tcps_transform, stlc.language)
}