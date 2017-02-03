package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Names._
import system.{Names, Transformation}

import scala.collection.immutable.ListMap

object ecps extends Transformation(stlc.language + tcps + ccps) {

  override val soundnessTimeout: Int = 600
  override val wellformednessTimeout: Int = 120

  // CPS expression transformation ecps
  val ecps = Symbol("ecps", in = List(Exp, Typ, Ctx, Typ), out = Exp, constr = false)

  private val omega = Var("omega", Typ)

  override val contract =
    Lemma("T-ecps",
      Judg(Typed,
        ccps(Var("C", Ctx), omega),
        ecps(Var("e", Exp), omega, Var("C", Ctx), Var("T", Typ)),
        Arr(Arr(tcps(Var("T", Typ), omega), omega), omega)
      ),
      // if ----------------
      Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T", Typ)),
      Judg(CtxOk, Var("C", Ctx)),
      Judg(TOk, omega)
    ) -> 1

  val ecps_ref = Rewrite(
    ecps(
      ref(Var("x", Name)),
      omega,
      Var("C", Ctx),
      Var("T", Typ)
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T", Typ), omega), omega),
      app(ref(Var("k", Name)), ref(Var("x", Name)))
    ),
    where = Seq(
      Judg(equ(Name), Var("k", Name), fresh(Ctx)(Var("C", Ctx)))
    )
  )
  // 'Goal', 'Typed-app', 'Lookup-Found', 'Typed-true-INV', 'Notin-empty', 'Notin-bind', 'Typed-lam', 'Lookup-true-INV', 'EQ-ref', 'dom-Exp', 'DIFF-ref-app', 'DIFF-ref-lam', 'DIFF-ref-add', 'DIFF-ref-num', 'DIFF-empty-bind', 'Lookup-Notin', 'Typed-ref', 'dom-Ctx', 'EQ-bind', 'freshCtx-notinCtx', 'notinCtx-true-INV', 'Lookup-ccps', 'Lookup-Next'

  val ecps_num = Rewrite(
    ecps(
      num(Var("n", Num)),
      omega,
      Var("C", Ctx),
      Var("T", Typ)
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Nat(), omega), omega),
      app(ref(Var("k", Name)), num(Var("n", Num)))
    ),
    where = Seq(
      Judg(equ(Name), Var("k", Name), fresh(Ctx)(Var("C", Ctx)))
    )
  )
  // 'EQ-Arr', 'Notin-empty', 'dom-Typ', 'TOk-Arr', 'TOk-tcps', 'tcps-1', 'TOk-true-INV', 'TOk-Nat', 'DIFF-Nat-Arr', 'notinCtx-true-INV', 'freshCtx-notinCtx', 'DIFF-num-lam', 'DIFF-num-app', 'DIFF-ref-num', 'Typed-true-INV', 'Goal', 'tcps-0', 'Typed-app', 'Typed-num', 'Typed-ref', 'Lookup-Found', 'Typed-lam'

  val ecps_add = Rewrite(
    ecps(
      add(Var("e1", Exp), Var("e2", Exp)),
      omega,
      Var("C", Ctx),
      Var("T", Typ)
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(Nat(), omega),
      app(ecps(Var("e1", Exp), omega, Var("C", Ctx), Var("T", Typ)),
        lam(Var("x1", Name), Nat(),
          app(ecps(Var("e2", Exp), omega, Var("C", Ctx), Var("T", Typ)),
            lam(Var("x2", Name), Nat(),
              app(ref(Var("k", Name)),
                add(ref(Var("x1", Name)), ref(Var("x2", Name))))))))
    ),
    where = Seq(
      Judg(equ(Name), Var("k", Name), fresh(Ctx)(Var("C", Ctx))),
      Judg(equ(Name), Var("x1", Name), fresh(Ctx)(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega)))),
      Judg(equ(Name), Var("x2", Name), fresh(Ctx)(bind(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega)), Var("x1", Name), Nat())))
    )
  )

  val ecps_lam = Rewrite(
    ecps(
      lam(Var("x", Name), Var("T1", Typ), Var("e1", Exp)),
      omega,
      Var("C", Ctx),
      Var("T", Typ)
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T", Typ), omega), omega),
      app(ref(Var("k", Name)),
        lam(Var("x", Name), tcps(Var("T1", Typ), omega),
          ecps(
            Var("e1", Exp),
            omega,
            bind(Var("C", Ctx), Var("x", Name), Var("T1", Typ)),
            Var("T2", Typ))))
    ),
    where = Seq(
      Judg(equ(Name), Var("k", Name), fresh(Ctx)(bind(Var("C", Ctx), Var("x", Name), Var("T1", Typ)))),
      Judg(equ(Typ), Arr(Var("T1", Typ), Var("T2", Typ)), Var("T", Typ))
    )
  )

  val ecps_app = Rewrite(
    ecps(
      app(Var("e1", Exp), Var("e2", Exp)),
      omega,
      Var("C", Ctx),
      Var("T", Typ)
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T", Typ), omega), omega),
      app(ecps(Var("e1", Exp), omega, Var("C", Ctx), Arr(Var("T1", Typ), Var("T", Typ))),
        lam(Var("xf", Name), tcps(Arr(Var("T1", Typ), Var("T", Typ)), omega),
          app(ecps(Var("e2", Exp), omega, Var("C", Ctx), Var("T1", Typ)),
            lam(Var("xv", Name), tcps(Var("T1", Typ), omega),
              app(
                app(ref(Var("xf", Name)), ref(Var("xv", Name))),
                ref(Var("k", Name)))))))
    ),
    where = Seq(
      Judg(TOk, Var("T1", Typ)),
      Judg(Typed, Var("C", Ctx), Var("e1", Exp), Arr(Var("T1", Typ), Var("T", Typ))),
      Judg(equ(Name), Var("k", Name), fresh(Ctx)(Var("C", Ctx))),
      Judg(equ(Name), Var("xf", Name), fresh(Ctx)(bind(Var("C", Ctx), Var("k", Name), Arr(tcps(Var("T", Typ), omega), omega)))),
      Judg(equ(Name), Var("xv", Name), fresh(Ctx)(bind(bind(Var("C", Ctx), Var("k", Name), Arr(tcps(Var("T", Typ), omega), omega)), Var("xf", Name), tcps(Arr(Var("T1", Typ), Var("T", Typ)), omega))))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(
    ecps_ref,
    ecps_num,
    ecps_add,
    ecps_lam,
    ecps_app
  )

  checkSyntax()
}


/*
 * Bugs found during development:
 * 1) Contract of ecps was wrong. Instead of `Arr(Arr(tcps(Var("T", Typ), omega), omega), omega)` I only had `tcps(Var("T", Typ), omega)` at first
 * 2) freshness condition of ecps-app were wrong, using wrong types for k and vf (copied from ecps-add)
 */