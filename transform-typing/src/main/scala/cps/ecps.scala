package cps

import stlc.Syntax._
import stlc.Statics._
import system.Syntax._
import system.Names._
import system.{Names, Transformation}

import scala.collection.immutable.ListMap

object ecps extends Transformation(stlc.language + tcps + ccps) {

  // CPS expression transformation ecps
  val ecps = Symbol("ecps", in = List(Exp, Typ), out = Exp)

  private val omega = Var("omega", Typ)

  override val contract: Rule = Rule("T-ecps",
    Judg(Typed,
      ccps(Var("C", Ctx), omega),
      ecps(Var("e", Exp), omega),
      tcps(Var("T", Typ), omega)
    ),
    // if ----------------
    Judg(Typed, Var("C", Ctx), Var("e", Exp), Var("T", Typ))
  )

  override val contractPos: Int = 1

  val ecps_ref = Rewrite(
    ecps(
      ref(Var("x", Name)),
      omega
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T", Typ), omega), omega),
      app(ref(Var("k", Name)), ref(Var("x", Name)))
    ),
    where = ListMap(
      Var("k", Name) -> fresh(Ctx)(Var("C", Ctx))
    )
  )

  val ecps_num = Rewrite(
    ecps(
      num(Var("n", Num)),
      omega
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Nat(), omega), omega),
      app(ref(Var("k", Name)), num(Var("n", Num)))
    ),
    where = ListMap(
      Var("k", Name) -> fresh(Ctx)(Var("C", Ctx))
    )
  )

  val ecps_add = Rewrite(
    ecps(
      add(Var("e1", Exp), Var("e2", Exp)),
      omega
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(Nat(), omega),
      app(ecps(Var("e1", Exp), omega),
        lam(Var("x1", Name), Nat(),
          app(ecps(Var("e2", Exp), omega),
            lam(Var("x2", Name), Nat(),
              app(ref(Var("k", Name)),
                add(ref(Var("x1", Name)), ref(Var("x2", Name))))))))
    ),
    where = ListMap(
      Var("k", Name) -> fresh(Ctx)(Var("C", Ctx)),
      Var("x1", Name) -> fresh(Ctx)(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega))),
      Var("x2", Name) -> fresh(Ctx)(bind(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega)), Var("x1", Name), Nat()))
    )
  )

  val ecps_lam = Rewrite(
    ecps(
      lam(Var("x", Name), Var("T1", Typ), Var("e1", Exp)),
      omega
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T", Typ), omega), omega),
      app(ref(Var("k", Name)),
        lam(Var("x", Name), tcps(Var("T1", Typ), omega),
          ecps(Var("e1", Exp), omega)))
    ),
    where = ListMap(
      Var("k", Name) -> fresh(Ctx)(Var("C", Ctx))
    )
  )

  val ecps_app = Rewrite(
    ecps(
      app(Var("e1", Exp), Var("e2", Exp)),
      omega
    ),
    // ~>
    lam(
      Var("k", Name),
      Arr(tcps(Var("T2", Typ), omega), omega),
      app(ecps(Var("e1", Exp), omega),
        lam(Var("xf", Name), tcps(Arr(Var("T1", Typ), Var("T2", Typ)), omega),
          app(ecps(Var("e2", Exp), omega),
            lam(Var("xv", Name), tcps(Var("T1", Typ), omega),
              app(
                app(ref(Var("xf", Name)), ref(Var("xv", Name))),
                ref(Var("k", Name)))))))
    ),
    where = ListMap(
      Arr(Var("T1", Typ), Var("T2", Typ)) -> Var("T", Typ),
      Var("k", Name) -> fresh(Ctx)(Var("C", Ctx)),
      Var("xf", Name) -> fresh(Ctx)(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega))),
      Var("xv", Name) -> fresh(Ctx)(bind(bind(Var("C", Ctx), Var("k", Name), Arr(Nat(), omega)), Var("xf", Name), Nat()))
    )
  )

  override val rewrites: Seq[Rewrite] = Seq(
    ecps_ref,
    ecps_num,
    ecps_add,
    ecps_lam,
    ecps_app
  )
}