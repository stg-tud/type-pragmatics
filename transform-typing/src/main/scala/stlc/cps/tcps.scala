package stlc.cps

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

import scala.collection.immutable.ListMap

object tcps extends Transformation(stlc.language) {

  // CPS type transformation tcps
  val tcps = Symbol("tcps", in = List(Typ, Typ), out = Typ, constr = false)

  private val omega = "omega"~Typ

  override val contract =
    Lemma("TOk-tcps",
      Judg(TOk, tcps("T"~Typ, omega)),
      // if ----------------
      Judg(TOk, "T"~Typ),
      Judg(TOk, omega)
    ) -> 0



  val tcps_nat = Rewrite(
    tcps(Nat(), omega),
    // ~>
    Nat()
  )

  val tcps_arr = Rewrite(
    tcps(Arr("T1"~Typ, "T2"~Typ), omega),
    // ~>
    Arr(tcps("T1"~Typ, omega),
        Arr(
          Arr(tcps("T2"~Typ, omega), omega),
          omega
        )
    )
  )

  override val rewrites = Seq(tcps_nat, tcps_arr)

  checkSyntax()
}