package cps

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

import scala.collection.immutable.ListMap

object tcps extends Transformation(stlc.language) {

  // CPS type transformation tcps
  val tcps = Symbol("tcps", in = List(Typ, Typ), out = Typ, constr = false)

  private val omega = Var("omega", Typ)

  override val contracts = ListMap(
    Lemma("TOk-tcps",
      Judg(TOk, App(tcps, Var("T", Typ), omega)),
      // if ----------------
      Judg(TOk, Var("T", Typ)),
      Judg(TOk, omega)
    ) -> 0
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

  override val rewrites = Seq(tcps_nat, tcps_arr)

  checkSyntax()
}