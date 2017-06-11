package whilelang.matrix

import whilelang.Syntax._
import whilelang.Statics._
import system.Syntax._
import system.{Names, Transformation}
import system.Names._

object matrix_desugar extends Transformation(whilelang.language + matrix_ext) {

  val desugar = Symbol("desugar", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract: (Rule, Int) =
    Lemma("Typed-desugar",
      Judg(Typed, "C"~Ctx, desugar("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
    ) -> 1


  // desugar constructs of matrix extension
  val desugar_newmatrix = Rewrite(
    desugar(newmatrix("m"~Num, "n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    /*
    Vec<Vec<Dbl>> result = new Vector(m);
    double i = 0;
    while (i < m) {
      result[i] = new Vector(n);
      i = i + 1;
    }
    result.closedMatrix(m, n)
    */
    block(
      declare("result"~Name, Vec(Vec(Dbl())), vecnew(num("m"~Num), Vec(Dbl())), seq(
        declare("i"~Name, Dbl(), num(zero()),
          loop(binop(ref("i"~Name), lt(), num("m"~Num)), seq(
            vecwrite(ref("result"~Name), ref("i"~Name), vecnew(num("n"~Num), Dbl())),
            assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
          ))
        ),
        exp(closedMatrix("m"~Num, "n"~Num, ref("result"~Name)))
      ))
    ),
    where = Seq(
      Judg(equ(Name), "result"~Name, fresh(Ctx)("C"~Ctx)),
      Judg(equ(Name), "i"~Name, fresh(Ctx)(bind("C"~Ctx, "result"~Name, Vec(Vec(Dbl())))))
    )
  )


  // traverse constructs of base language
  val desugar_vtrue = Rewrite(
    desugar(vtrue(), "C"~Ctx, "T"~Typ),
    // ~>
    vtrue()
  )
  val desugar_vfalse = Rewrite(
    desugar(vfalse(), "C"~Ctx, "T"~Typ),
    // ~>
    vfalse()
  )
  val desugar_ref = Rewrite(
    desugar(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref("x"~Name)
  )
  val desugar_num = Rewrite(
    desugar(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )
  val desugar_num_binop = Rewrite(
    desugar(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Dbl()),
    // ~>
    binop(desugar("e1"~Exp, "C"~Ctx, Dbl()), "op"~BinOp, desugar("e2"~Exp, "C"~Ctx, Dbl())),
    where = Seq(
      mkOr(Seq(
        equ(BinOp)("op"~BinOp, add()),
        equ(BinOp)("op"~BinOp, sub()),
        equ(BinOp)("op"~BinOp, mul()),
        equ(BinOp)("op"~BinOp, div())
      )).asInstanceOf[App].toJudg,
      Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
    )
  )
  val desugar_bool_binop = Rewrite(
    desugar(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(desugar("e1"~Exp, "C"~Ctx, Bool()), "op"~BinOp, desugar("e2"~Exp, "C"~Ctx, Bool())),
    where = Seq(
      Judg(OR,
        equ(BinOp)("op"~BinOp, and()),
        equ(BinOp)("op"~BinOp, or())
      ),
      Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
    )
  )
  val desugar_num_compare = Rewrite(
    desugar(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(desugar("e1"~Exp, "C"~Ctx, Dbl()), "op"~BinOp, desugar("e2"~Exp, "C"~Ctx, Dbl())),
    where = Seq(
      mkOr(Seq(
        equ(BinOp)("op"~BinOp, eqop()),
        equ(BinOp)("op"~BinOp, lt()),
        equ(BinOp)("op"~BinOp, gt())
      )).asInstanceOf[App].toJudg,
      Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
    )
  )
  val desugar_bool_equals = Rewrite(
    desugar(binop("e1"~Exp, eqop(), "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(desugar("e1"~Exp, "C"~Ctx, Bool()), eqop(), desugar("e2"~Exp, "C"~Ctx, Bool())),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
    )
  )
  val desugar_neg = Rewrite(
    desugar(unop(neg(), "e"~Exp), "C"~Ctx, Dbl()),
    // ~>
    unop(neg(), desugar("e"~Exp, "C"~Ctx, Dbl()))
  )
  val desugar_not = Rewrite(
    desugar(unop(not(), "e"~Exp), "C"~Ctx, Bool()),
    // ~>
    unop(not(), desugar("e"~Exp, "C"~Ctx, Bool()))
  )
  val desugar_vecnew = Rewrite(
    desugar(vecnew("size"~Exp, "Te"~Typ), "C"~Ctx, "T"~Typ),
    // ~>
    vecnew(desugar("size"~Exp, "C"~Ctx, Dbl()), "Te"~Typ)
  )
  val desugar_vecread = Rewrite(
    desugar(vecread("e"~Exp, "ix"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    vecread(desugar("e"~Exp, "C"~Ctx, Vec("T"~Typ)), desugar("ix"~Exp, "C"~Ctx, Dbl()))
  )


  override val rewrites: Seq[Rewrite] = Seq(
    desugar_newmatrix

//    ,
//    desugar_vtrue,
//    desugar_vfalse,
//    desugar_ref,
//    desugar_num,
//    desugar_num_binop,
//    desugar_bool_binop,
//    desugar_num_compare,
//    desugar_bool_equals,
//    desugar_neg,
//    desugar_not,
//    desugar_vecnew,
//    desugar_vecread
  )

  checkSyntax()
}

object Run extends scala.App {
  lazy val compliant = matrix_desugar.isContractCompliant
  lazy val complete = matrix_desugar.isComplete
  lazy val sound = matrix_desugar.isSound

//  println(s"Compliant = $compliant")
//  println(s"Complete = $complete")
  println(s"Sound = $sound")
}
