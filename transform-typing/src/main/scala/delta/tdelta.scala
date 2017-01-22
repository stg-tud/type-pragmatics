package delta

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

object tdelta extends Transformation(stlc.language) {

  val tdelta = Symbol("tdelta", in = List(Typ), out = Typ, constr = false)

  override val contract: (Rule, Int) =
    Lemma("TOk-tdelta",
      Judg(TOk, tdelta("T"~Typ)),
      // if ----------------
      Judg(TOk, "T"~Typ)
    ) -> 0

  val tdelta_nat = Rewrite(
    tdelta(Nat()),
    // ~>
    Nat()
  )

  val tdelta_arr = Rewrite(
    tdelta(Arr("T1"~Typ, "T2"~Typ)),
    // ~>
    Arr("T1"~Typ, Arr(tdelta("T1"~Typ), tdelta("T2"~Typ)))
  )

  override val rewrites: Seq[Rewrite] = Seq(tdelta_nat, tdelta_arr)
}
