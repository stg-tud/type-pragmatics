package delta

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

object cdelta extends Transformation(stlc.language + tdelta) {

  val cdelta = Symbol("cdelta", in = List(Ctx), out = Ctx, constr = false)

  val d = Symbol("d", in = List(Name), out = Name, constr = true)
  val v = Symbol("v", in = List(Name), out = Name, constr = true)

  override val extraSymbols: Seq[Symbol] = Seq(d, v)

  override val contract: (Rule, Int) =
    Rule("CtxOk_cdelta",
      Judg(CtxOk, cdelta("C"~Ctx)),
      // if ----------------
      Judg(CtxOk, "C"~Ctx)
    ) -> 0

  val cdelta_empty = Rewrite(
    cdelta(empty()),
    // ~>
    empty()
  )

  val cdelta_bind = Rewrite(
    cdelta(bind("C"~Ctx, "x"~Name, "T"~Typ)),
    // ~>
    bind(bind(cdelta("C"~Ctx), v("x"~Name), "T"~Typ), d("x"~Name), tdelta("T"~Typ))
  )

  override val rewrites: Seq[Rewrite] = Seq(cdelta_empty, cdelta_bind)
}
