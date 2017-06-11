package whilelang

import whilelang.Statics._
import whilelang.Syntax._
import system.LanguageExtension
import system.Syntax._

package object matrix {
  val Matrix = Symbol("Matrix", in = List(Num, Num), out = Typ)

  val newmatrix = Symbol("newmatrix", in = List(Num, Num), out = Exp)
  val openMatrix = Symbol("openMatrix", in = List(Exp), out = Exp)
  val closedMatrix = Symbol("closedMatrix", in = List(Num, Num, Exp), out = Exp)
  val transpose = Symbol("transpose", in = List(), out = UnOp)

  val TOk_Matrix = Rule("TOk-Matrix",
    Judg(TOk, Matrix("m"~Num, "n"~Num))
  )

  val Typed_closedMatrix = Rule("Typed-closedMatrix",
    Judg(Typed, "C"~Ctx, closedMatrix("m"~Num, "n"~Num, "e"~Exp), Matrix("m"~Num, "n"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Vec(Vec(Dbl())))
  )
  val Typed_openMatrix = Rule("Typed-openMatrix",
    Judg(Typed, "C"~Ctx, openMatrix("e"~Exp), Vec(Vec(Dbl()))),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Matrix("m"~Num, "n"~Num))
  )

  val Typed_newmatrix = Rule("Typed-newmatrix",
    Judg(Typed, "C"~Ctx, newmatrix("m"~Num, "n"~Num), Matrix("m"~Num, "n"~Num))
    // if ----------------
  )
  val Typed_matrix_add = Rule("Typed-matrix-add",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, add(), "e2"~Exp), Matrix("m"~Num, "n"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix("m"~Num, "n"~Num)),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix("m"~Num, "n"~Num))
  )

  val Typed_matrix_mul_scalar_left = Rule("Typed-matrix-mul-scalar-left",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix("m"~Num, "n"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix("m"~Num, "n"~Num))
  )

  val Typed_matrix_mul_scalar_right = Rule("Typed-matrix-mul-scalar-right",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix("m"~Num, "n"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix("m"~Num, "n"~Num)),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
  )

  val Typed_matrix_mul = Rule("Typed-matrix-add",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, mul(), "e2"~Exp), Matrix("m"~Num, "n"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix("m"~Num, "x"~Num)),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix("x"~Num, "n"~Num))
  )

  val Typed_matrix_transpose = Rule("Typed-matrix-transpose",
    Judg(Typed, "C"~Ctx, unop(transpose(), "e"~Exp), Matrix("n"~Num, "m"~Num)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Matrix("m"~Num, "n"~Num))
  )


  object matrix_ext extends LanguageExtension("matrix-ext",
    Seq(),
    Seq(Matrix, closedMatrix, openMatrix, newmatrix, transpose),
    Seq(
      TOk_Matrix,
      Typed_closedMatrix,
      Typed_openMatrix,
      Typed_newmatrix,
      Typed_matrix_add,
      Typed_matrix_mul,
      Typed_matrix_mul_scalar_left,
      Typed_matrix_mul_scalar_right,
      Typed_matrix_transpose
    )
  )

}
