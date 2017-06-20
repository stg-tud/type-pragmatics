package whilelang

import whilelang.Statics._
import whilelang.Syntax._
import system.LanguageExtension
import system.Syntax._

package object matrix {
  val Matrix = Symbol("Matrix", in = List(), out = Typ)

  val newmatrix = Symbol("newmatrix", in = List(Num, Num), out = Exp)
  val openMatrix = Symbol("openMatrix", in = List(Exp), out = Exp)
  val closeMatrix = Symbol("closeMatrix", in = List(Exp), out = Exp)
  val transpose = Symbol("transpose", in = List(), out = UnOp)

  val TOk_Matrix = Rule("TOk-Matrix",
    Judg(TOk, Matrix())
  )

  val Typed_closeMatrix = Rule("Typed-closeMatrix",
    Judg(Typed, "C"~Ctx, closeMatrix("e"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Vec(Vec(Dbl())))
  )
  val Typed_openMatrix = Rule("Typed-openMatrix",
    Judg(Typed, "C"~Ctx, openMatrix("e"~Exp), Vec(Vec(Dbl()))),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Matrix())
  )

  val Typed_newmatrix = Rule("Typed-newmatrix",
    Judg(Typed, "C"~Ctx, newmatrix("m"~Num, "n"~Num), Matrix())
    // if ----------------
  )
  val Typed_matrix_add = Rule("Typed-matrix-add",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, add(), "e2"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix())
  )

  val Typed_matrix_mul_scalar_left = Rule("Typed-matrix-mul-scalar-left",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix())
  )

  val Typed_matrix_mul_scalar_right = Rule("Typed-matrix-mul-scalar-right",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
  )

  val Typed_matrix_mul = Rule("Typed-matrix-mul",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix())
  )

  val Typed_matrix_transpose = Rule("Typed-matrix-transpose",
    Judg(Typed, "C"~Ctx, unop(transpose(), "e"~Exp), Matrix()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Matrix())
  )


  // specialized inversion axioms
  val Typed_inv_vecread = Lemma("Typed-Inv-vecread",
    Judg(AND,
      Judg(Typed, "C"~Ctx, "e"~Exp, Vec("T"~Typ)).toApp,
      Judg(Typed, "C"~Ctx, "ix"~Exp, Dbl()).toApp
    ),
    // if ----------------
    Judg(Typed, "C"~Ctx, vecread("e"~Exp, "ix"~Exp), "T"~Typ)
  )
//  val Typed_inv_add = Lemma("Typed-Inv-add",
//    Judg(OR,
//      mkAnd(Seq(
//        Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()).toApp,
//        Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl()).toApp,
//        Judg(equ(Typ), "T"~Typ, Dbl()).toApp
//      )),
//      mkExists(Seq("m"~Num, "n"~Num),
//        mkAnd(Seq(
//          Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()).toApp,
//          Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix()).toApp,
//          Judg(equ(Typ), "T"~Typ, Matrix()).toApp
//        ))
//      )
//    ),
//    // if ----------------
//    Judg(Typed, "C"~Ctx, binop("e1"~Exp, add(), "e2"~Exp), "T"~Typ)
//  )


  object matrix_ext extends LanguageExtension("matrix-ext",
    Seq(),
    Seq(Matrix, closeMatrix, openMatrix, newmatrix, transpose),
    Seq(
      TOk_Matrix,
      Typed_closeMatrix,
      Typed_openMatrix,
      Typed_newmatrix,
      Typed_matrix_add,
      Typed_matrix_mul,
      Typed_matrix_mul_scalar_left,
      Typed_matrix_mul_scalar_right,
      Typed_matrix_transpose,

      Typed_inv_vecread
    )
  )

}
