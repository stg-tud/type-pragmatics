package whilelang.matrix

import whilelang.Syntax._
import whilelang.Statics._
import system.Syntax._
import system.{Names, Transformation}
import system.Names._



object compile extends Transformation(whilelang.language + matrix_ext) {

  val compile = Symbol("compile", in = List(Exp, Ctx, Typ), out = Exp, constr = false)

  override val contract: (Rule, Int) =
    Lemma("Typed-compile",
      Judg(Typed, "C"~Ctx, compile("e"~Exp, "C"~Ctx, "T"~Typ), "T"~Typ),
      // if ----------------
      Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
    ) -> 1


  // compile constructs of matrix extension
  val compile_newmatrix = Rewrite(
    compile(newmatrix("e1"~Exp, "e2"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    /*
    double m = `e1`;
    double n = `e2`;
    Vec<Vec<Dbl>> result = new Vector(m);
    double i = 0;
    while (i < m) {
      result[i] = new Vector(n);
      i = i + 1;
    }
    result.closeMatrix()
    */
    block(
      declare("m"~Name, Dbl(), compile("e1"~Exp, "C"~Ctx, Dbl()),
        declare("n"~Name, Dbl(), compile("e2"~Exp, "C"~Ctx, Dbl()),
          declare("result"~Name, Vec(Vec(Dbl())), vecnew(ref("m"~Name), Vec(Dbl())), seq(
            declare("i"~Name, Dbl(), num(zero()),
              loop(binop(ref("i"~Name), lt(), ref("m"~Name)), seq(
                vecwrite(ref("result"~Name), ref("i"~Name), vecnew(ref("n"~Name), Dbl())),
                assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
              ))
            ),
            exp(closeMatrix(ref("result"~Name)))
          ))
        )
      )
    ),
    where = mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
      "m" -> Dbl(),
      "n" -> Dbl(),
      "result" -> Vec(Vec(Dbl())),
      "i" -> Dbl()
    ))
  )

  val compile_matrix_add = Rewrite(
    compile(binop("e1"~Exp, add(), "e2"~Exp), "C"~Ctx, Matrix()),
    // ~>
    /*
    Vec<Vec<Dbl>> v1 = `e1`.openMatrix();
    Vec<Vec<Dbl>> v2 = `e2`.openMatrix();
    double i = 0;
    double j = 0;
    while (i < m) {
      while (j < n) {
        v1[i][j] = v1[i][j] + v2[i][j]
        j = j + 1;
      }
      i = i + 1;
    }
    v1.closedMatrix(m, n)
    */
    block(
      declare("v1"~Name, Vec(Vec(Dbl())), openMatrix(compile("e1"~Exp, "C"~Ctx, Matrix())), seq(
        declare("v2"~Name, Vec(Vec(Dbl())), openMatrix(compile("e2"~Exp, "C"~Ctx, Matrix())),
          declare("i"~Name, Dbl(), num(zero()),
            declare("j"~Name, Dbl(), num(zero()),
              loop(binop(ref("i"~Name), lt(), veclength(ref("v1"~Name))), seq(
                loop(binop(ref("j"~Name), lt(), veclength(vecread(ref("v1"~Name), ref("i"~Name)))), seq(
                  vecwrite(vecread(ref("v1"~Name), ref("i"~Name)), ref("j"~Name),
                    binop(
                      vecread(vecread(ref("v1"~Name), ref("i"~Name)), ref("j"~Name)),
                      add(),
                      vecread(vecread(ref("v2"~Name), ref("i"~Name)), ref("j"~Name))
                    )
                  ),
                  assign("j"~Name, binop(ref("j"~Name), add(), num(one())))
                )),
                assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
              ))
            )
          )
        ),
        exp(closeMatrix(ref("v1"~Name)))
      ))
    ),
    where = {
      Seq(
        Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()),
        Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix())
      ) ++ mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
        "v1" -> Vec(Vec(Dbl())),
        "v2" -> Vec(Vec(Dbl())),
        "i" -> Dbl(),
        "j" -> Dbl()
      ))
    }
  )

  val compile_matrix_mul_scalar_left = Rewrite(
    compile(binop("e1"~Exp, mul(), "e2"~Exp), "C"~Ctx, Matrix()),
    // ~>
    /*
    Dbl s = `e1`;
    Vec<Vec<Dbl>> v = `e2`.openMatrix();
    double i = 0;
    double j = 0;
    while (i < m) {
      while (j < n) {
        v[i][j] = s * v[i][j]
        j = j + 1;
      }
      i = i + 1;
    }
    v.closedMatrix(m, n)
    */
    block(
      declare("s"~Name, Dbl(), compile("e1"~Exp, "C"~Ctx, Dbl()),
        declare("v"~Name, Vec(Vec(Dbl())), openMatrix(compile("e2"~Exp, "C"~Ctx, Matrix())), seq(
          declare("i"~Name, Dbl(), num(zero()),
            declare("j"~Name, Dbl(), num(zero()),
              loop(binop(ref("i"~Name), lt(), veclength(ref("v"~Name))), seq(
                loop(binop(ref("j"~Name), lt(), veclength(vecread(ref("v"~Name), ref("i"~Name)))), seq(
                  vecwrite(vecread(ref("v"~Name), ref("i"~Name)), ref("j"~Name),
                    binop(
                      ref("s"~Name),
                      mul(),
                      vecread(vecread(ref("v"~Name), ref("i"~Name)), ref("j"~Name))
                    )
                  ),
                  assign("j"~Name, binop(ref("j"~Name), add(), num(one())))
                )),
                assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
              ))
            )
          ),
          exp(closeMatrix(ref("v"~Name)))
        ))
      )
    ),
    where = {
      Seq(
        Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
        Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix())
      ) ++ mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
        "s" -> Dbl(),
        "v" -> Vec(Vec(Dbl())),
        "i" -> Dbl(),
        "j" -> Dbl()
      ))
    }
  )

  val compile_matrix_mul_scalar_right = Rewrite(
    compile(binop("e1"~Exp, mul(), "e2"~Exp), "C"~Ctx, Matrix()),
    // ~>
    /*
    Vec<Vec<Dbl>> v = `e1`.openMatrix();
    Dbl s = `e2`;
    double i = 0;
    double j = 0;
    while (i < m) {
      while (j < n) {
        v[i][j] = s * v[i][j]
        j = j + 1;
      }
      i = i + 1;
    }
    v.closedMatrix(m, n)
    */
    block(
      declare("v"~Name, Vec(Vec(Dbl())), openMatrix(compile("e1"~Exp, "C"~Ctx, Matrix())), seq(
        declare("s"~Name, Dbl(), compile("e2"~Exp, "C"~Ctx, Dbl()),
          declare("i"~Name, Dbl(), num(zero()),
            declare("j"~Name, Dbl(), num(zero()),
              loop(binop(ref("i"~Name), lt(), veclength(ref("v"~Name))), seq(
                loop(binop(ref("j"~Name), lt(), veclength(vecread(ref("v"~Name), ref("i"~Name)))), seq(
                  vecwrite(vecread(ref("v"~Name), ref("i"~Name)), ref("j"~Name),
                    binop(
                      ref("s"~Name),
                      mul(),
                      vecread(vecread(ref("v"~Name), ref("i"~Name)), ref("j"~Name))
                    )
                  ),
                  assign("j"~Name, binop(ref("j"~Name), add(), num(one())))
                )),
                assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
              ))
            )
          )
        ),
        exp(closeMatrix(ref("v"~Name)))
      ))
    ),
    where = {
      Seq(
        Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix()),
        Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
      ) ++ mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
        "s" -> Dbl(),
        "v" -> Vec(Vec(Dbl())),
        "i" -> Dbl(),
        "j" -> Dbl()
      ))
    }
  )

  val compile_matrix_mul = Rewrite(
    compile(binop("e1"~Exp, mul(), "e2"~Exp), "C"~Ctx, Matrix()),
    // ~>
    /*
    Vec<Vec<Dbl>> v1 = compile(`e1`).openMatrix();
    Vec<Vec<Dbl>> v2 = compile(`e2`).openMatrix();
    Vec<Vec<Dbl>> result = new Vec(v1.length);
    double i = 0;
    double j = 0;
    while (i < v1.length) {
      result[i] = new Vec(v2[0].length);
      while (j < v2[0].length) {
        double r = 0;
        double cell = 0;
        while (r < v2.length) {
          cell = cell + (v1[i][r] * v2[r][j])
          r = r + 1;
        }
        result[i][j] = cell;
        j = j + 1;
      }
      i = i + 1;
    }
    result.closedMatrix()
    */
    block(
      declare("v1"~Name, Vec(Vec(Dbl())), openMatrix(compile("e1"~Exp, "C"~Ctx, Matrix(/*"m"~Num, "x"~Num*/))),
        declare("v2"~Name, Vec(Vec(Dbl())), openMatrix(compile("e2"~Exp, "C"~Ctx, Matrix(/*"x"~Num, "n"~Num*/))),
          declare("result"~Name, Vec(Vec(Dbl())), vecnew(veclength(ref("v1"~Name)), Vec(Dbl())), seq(
            declare("i"~Name, Dbl(), num(zero()),
              declare("j"~Name, Dbl(), num(zero()),
                loop(binop(ref("i"~Name), lt(), veclength(ref("v1"~Name))), seq(seq(
                  vecwrite(ref("result"~Name), ref("i"~Name), vecnew(veclength(vecread(ref("v2"~Name), num(zero()))), Dbl())),
                  loop(binop(ref("j"~Name), lt(), veclength(vecread(ref("v2"~Name), num(zero())))), seq(
                    declare("r"~Name, Dbl(), num(zero()),
                      declare("cell"~Name, Dbl(), num(zero()), seq(
                        loop(binop(ref("r"~Name), lt(), veclength(ref("v2"~Name))), seq(
                          assign("cell"~Name, binop(ref("cell"~Name), add(),
                            binop(
                              vecread(vecread(ref("v1"~Name), ref("i"~Name)), ref("r"~Name)),
                              mul(),
                              vecread(vecread(ref("v2"~Name), ref("r"~Name)), ref("j"~Name))
                            )
                          )),
                          assign("r"~Name, binop(ref("r"~Name), add(), num(one())))
                        )),
                        vecwrite(vecread(ref("result"~Name), ref("i"~Name)), ref("j"~Name), ref("cell"~Name))
                      ))
                    ),
                    assign("j"~Name, binop(ref("j"~Name), add(), num(one())))
                  ))),
                  assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
                ))
              )
            ),
            exp(closeMatrix(ref("result"~Name)))
          ))
        )
      )
    ),
    where = {
      Seq(
        Judg(Typed, "C"~Ctx, "e1"~Exp, Matrix(/*"m"~Num, "x"~Num*/)),
        Judg(Typed, "C"~Ctx, "e2"~Exp, Matrix(/*"x"~Num, "n"~Num*/))
      ) ++ mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
        "v1" -> Vec(Vec(Dbl())),
        "v2" -> Vec(Vec(Dbl())),
        "result" -> Vec(Vec(Dbl())),
        "i" -> Dbl(),
        "j" -> Dbl(),
        "r" -> Dbl(),
        "cell" -> Dbl()
      ))
    }
  )

  val compile_matrix_transpose = Rewrite(
    compile(unop(transpose(), "e"~Exp), "C"~Ctx, Matrix(/*"n"~Num, "m"~Num*/)),
    // ~>
    /*
    Vec<Vec<Dbl>> v = `e`.openMatrix();
    Vec<Vec<Dbl>> result = new Vec(v[zero].length);
    double i = 0;
    double j = 0;
    while (i < n) {
      result[i] = new Vec(m);
      while (j < m) {
        result[i][j] = v[j][i];
        j = j + 1;
      }
      i = i + 1;
    }
    v1.closedMatrix(m, n)
    */
    block(
      declare("v"~Name, Vec(Vec(Dbl())), openMatrix(compile("e"~Exp, "C"~Ctx, Matrix())),
        declare("result"~Name, Vec(Vec(Dbl())), vecnew(veclength(vecread(ref("v"~Name), num(zero()))), Vec(Dbl())), seq(
          declare("i"~Name, Dbl(), num(zero()),
            declare("j"~Name, Dbl(), num(zero()),
              loop(binop(ref("i"~Name), lt(), veclength(vecread(ref("v"~Name), num(zero())))), seq(seq(
                vecwrite(ref("result"~Name), ref("i"~Name), vecnew(veclength(ref("v"~Name)), Dbl())),
                loop(binop(ref("j"~Name), lt(), veclength(ref("v"~Name))), seq(
                  vecwrite(
                    vecread(ref("result"~Name), ref("i"~Name)), ref("j"~Name),
                    vecread(vecread(ref("v"~Name), ref("j"~Name)), ref("i"~Name))
                  ),
                  assign("j"~Name, binop(ref("j"~Name), add(), num(one())))
                ))),
                assign("i"~Name, binop(ref("i"~Name), add(), num(one())))
              ))
            )
          ),
          exp(closeMatrix(ref("result"~Name)))
        ))
      )
    ),
    where = {
      Seq(
        Judg(Typed, "C"~Ctx, "e"~Exp, Matrix())
      ) ++ mkFreshJudgs(Ctx, "C"~Ctx, bind)(List(
        "v" -> Vec(Vec(Dbl())),
        "result" -> Vec(Vec(Dbl())),
        "i" -> Dbl(),
        "j" -> Dbl()
      ))
    }
  )

  val   compile_closeMatrix = Rewrite(
    compile(closeMatrix("e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    closeMatrix(compile("e"~Exp, "C"~Ctx, Vec(Vec(Dbl())))),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e"~Exp, Vec(Vec(Dbl())))
    )
  )
  val compile_openMatrix = Rewrite(
    compile(openMatrix("e"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    openMatrix(compile("e"~Exp, "C"~Ctx, Matrix())),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e"~Exp, Matrix())
    )
  )





  // traverse constructs of base language
  val compile_vtrue = Rewrite(
    compile(vtrue(), "C"~Ctx, "T"~Typ),
    // ~>
    vtrue()
  )
  val compile_vfalse = Rewrite(
    compile(vfalse(), "C"~Ctx, "T"~Typ),
    // ~>
    vfalse()
  )
  val compile_ref = Rewrite(
    compile(ref("x"~Name), "C"~Ctx, "T"~Typ),
    // ~>
    ref("x"~Name)
  )
  val compile_num = Rewrite(
    compile(num("n"~Num), "C"~Ctx, "T"~Typ),
    // ~>
    num("n"~Num)
  )
  val compile_num_binop = Rewrite(
    compile(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Dbl()),
    // ~>
    binop(compile("e1"~Exp, "C"~Ctx, Dbl()), "op"~BinOp, compile("e2"~Exp, "C"~Ctx, Dbl())),
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
  val compile_bool_binop = Rewrite(
    compile(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(compile("e1"~Exp, "C"~Ctx, Bool()), "op"~BinOp, compile("e2"~Exp, "C"~Ctx, Bool())),
    where = Seq(
      Judg(OR,
        equ(BinOp)("op"~BinOp, and()),
        equ(BinOp)("op"~BinOp, or())
      ),
      Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
    )
  )
  val compile_num_compare = Rewrite(
    compile(binop("e1"~Exp, "op"~BinOp, "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(compile("e1"~Exp, "C"~Ctx, Dbl()), "op"~BinOp, compile("e2"~Exp, "C"~Ctx, Dbl())),
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
  val compile_bool_equals = Rewrite(
    compile(binop("e1"~Exp, eqop(), "e2"~Exp), "C"~Ctx, Bool()),
    // ~>
    binop(compile("e1"~Exp, "C"~Ctx, Bool()), eqop(), compile("e2"~Exp, "C"~Ctx, Bool())),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
      Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
    )
  )
  val compile_neg = Rewrite(
    compile(unop(neg(), "e"~Exp), "C"~Ctx, Dbl()),
    // ~>
    unop(neg(), compile("e"~Exp, "C"~Ctx, Dbl()))
  )
  val compile_not = Rewrite(
    compile(unop(not(), "e"~Exp), "C"~Ctx, Bool()),
    // ~>
    unop(not(), compile("e"~Exp, "C"~Ctx, Bool()))
  )
  val compile_vecnew = Rewrite(
    compile(vecnew("size"~Exp, "Te"~Typ), "C"~Ctx, "T"~Typ),
    // ~>
    vecnew(compile("size"~Exp, "C"~Ctx, Dbl()), "Te"~Typ)
  )
  val compile_vecread = Rewrite(
    compile(vecread("e"~Exp, "ix"~Exp), "C"~Ctx, "T"~Typ),
    // ~>
    vecread(compile("e"~Exp, "C"~Ctx, Vec("T"~Typ)), compile("ix"~Exp, "C"~Ctx, Dbl()))
  )
  val compile_veclength = Rewrite(
    compile(veclength("e"~Exp), "C"~Ctx, Dbl()),
    // ~>
    veclength(compile("e"~Exp, "C"~Ctx, Vec("T"~Typ))),
    where = Seq(
      Judg(Typed, "C"~Ctx, "e"~Exp, Vec("T"~Typ))
    )
  )
  val compile_block = Rewrite(
    compile(block("s"~Stm), "C"~Ctx, "T"~Typ),
    // ~>
    block("s"~Stm)
  )


  override val rewrites: Seq[Rewrite] = Seq(
    compile_newmatrix,
    compile_matrix_add,
    compile_matrix_mul_scalar_left,
    compile_matrix_mul_scalar_right,
    compile_matrix_mul,
    compile_matrix_transpose,

    compile_closeMatrix,
    compile_openMatrix,

    compile_vtrue,
    compile_vfalse,
    compile_ref,
    compile_num,
    compile_num_binop,
    compile_bool_binop,
    compile_num_compare,
    compile_bool_equals,
    compile_neg,
    compile_not,
    compile_vecnew,
    compile_vecread,
    compile_veclength,
    compile_block
  )

  checkSyntax()
}