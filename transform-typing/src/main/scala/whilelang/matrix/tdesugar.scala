package whilelang.matrix

import system.Syntax._
import system.Transformation
import whilelang.Statics.Typed
import whilelang.Syntax._
import whilelang.Statics._
import whilelang.matrix.edesugar.edesugar

object tdesugar extends Transformation(whilelang.language + matrix_ext) {
  val tdesugar = Symbol("desugar", in = List(Typ), out = Typ, constr = false)

  override val contract: (Rule, Int) =
    Lemma("Typed-desugar",
      Judg(TOk, tdesugar("T"~Typ)),
      // if ----------------
      Judg(TOk, "T"~Typ)
    ) -> 0

  val tdesugar_Matrix = Rewrite(
    tdesugar(Matrix()),
    // ~>
    Vec(Vec(Dbl()))
  )

  val tdesugar_Void = Rewrite(
    tdesugar(Void()),
    // ~>
    Void()
  )

  val tdesugar_Dbl = Rewrite(
    tdesugar(Dbl()),
    // ~>
  Dbl()
  )

  val tdesugar_Bool = Rewrite(
    tdesugar(Bool()),
    // ~>
    Bool()
  )

  val tdesugar_Vec = Rewrite(
    tdesugar(Vec("T"~Typ)),
    // ~>
    Vec(tdesugar("T"~Typ))
  )

  val tdesugar_Arr = Rewrite(
    tdesugar(Arr("T1"~Typ, "T2"~Typ)),
    // ~>
    Arr(tdesugar("T1"~Typ), tdesugar("T2"~Typ))
  )

  override val rewrites: Seq[Rewrite] = Seq(
    tdesugar_Matrix,
    tdesugar_Void,
    tdesugar_Dbl,
    tdesugar_Bool,
    tdesugar_Vec,
    tdesugar_Arr
  )
}
