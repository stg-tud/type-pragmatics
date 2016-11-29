package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Transformation

object ecps extends Transformation(stlc.language + tcps + ccps) {

  // CPS expression transformation ecps
  val ecps = Symbol("ecps", in = List(Exp, Typ), out = Exp)

  private val omega = Var("omega", Typ)

  override val contract: Rule = Rule("T-ecps",
    Judg(Typed,
      App(ccps, Var("C", Ctx), omega),
      App(ecps, Var("e", Exp), omega),
      App(tcps, Var("T", Typ), omega)
    ),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T", Typ))
  )

  override val contractPos: Int = 1

  // TODO rewrites
  override val rewrites: Seq[Rewrite] = Seq()
}