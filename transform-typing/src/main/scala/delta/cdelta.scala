package delta

import stlc.Statics._
import stlc.Syntax._
import system.Syntax._
import system.Transformation

import scala.collection.immutable.ListMap

object cdelta extends Transformation(stlc.language + deltaext + tdelta) {

  val cdelta = Symbol("cdelta", in = List(Ctx), out = Ctx, constr = false)

  override val soundnessTimeout: Int = 90

  override val contract: (Rule, Int) =
    Lemma("CtxOk-cdelta",
      Judg(CtxOk, cdelta("C"~Ctx)),
      // if ----------------
      Judg(CtxOk, "C"~Ctx)
    ) -> 0

  override val lemmas = ListMap(
    Lemma("Lookup-cdelta-d",
      Judg(Lookup, d("x"~Name), tdelta("T"~Typ), cdelta("C"~Ctx)),
      // if ----------------
      Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx),
      Judg(TOk, "T"~Typ),
      Judg(CtxOk, "C"~Ctx)
    ) -> 2,

    Lemma("Lookup-cdelta-v",
      Judg(Lookup, v("x"~Name), "T"~Typ, cdelta("C"~Ctx)),
      // if ----------------
      Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx),
      Judg(CtxOk, "C"~Ctx)
    ) -> 2
  )

  val cdelta_empty = Rewrite(
    cdelta(empty()),
    // ~>
    empty()
  )

  val cdelta_bind = Rewrite(
    cdelta(bind("C"~Ctx, "y"~Name, "T"~Typ)),
    // ~>
    bind(bind(cdelta("C"~Ctx), v("y"~Name), "T"~Typ), d("y"~Name), tdelta("T"~Typ))
  )

  override val rewrites: Seq[Rewrite] = Seq(cdelta_empty, cdelta_bind)

  checkSyntax()
}
