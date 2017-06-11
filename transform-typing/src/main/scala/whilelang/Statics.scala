package whilelang

import system.Names.notin
import system.Syntax._
import whilelang.Syntax._

object Statics {

  val Lookup = symbol("Lookup", in = List(Name, Typ, Ctx), out = Prop)
  val Lookup_Found = rule("Lookup-Found",
    Judg(Lookup,
      "x"~Name,
      "T"~Typ,
      App(bind, "C"~Ctx, "x"~Name, "T"~Typ))
    // if ----------------
  )
  val Lookup_Next = rule("Lookup-Next",
    Judg(Lookup,
      "x"~Name,
      "T"~Typ,
      App(bind, "C"~Ctx, "y"~Name, "S"~Typ)),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx)
  )
  val Lookup_Bind_Inv = rule(Lemma("Lookup-Bind-Inv",
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, bind("C"~Ctx, "y"~Name, "Ty"~Typ)),
    Judg(neq(Name), "x"~Name, "y"~Name)
  ))

  val Notin_Empty = rule("Notin-empty",
    Judg(notin(Ctx), "x"~Name, empty())
    // if ----------------
  )
  val Notin_Bind = rule("Notin-bind",
    Judg(notin(Ctx), "x"~Name, bind("C"~Ctx, "y"~Name, "T"~Typ)),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(notin(Ctx), "x"~Name, "C"~Ctx)
  )

  val Lookup_Notin = rule(Lemma("Lookup-Notin",
    Judg(neq(Ctx), "C1"~Ctx, "C2"~Ctx),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, "C1"~Ctx),
    Judg(notin(Ctx), "x"~Name, "C2"~Ctx)
  ))

  val TOk = symbol("TOk", in = List(Typ), out = Prop)
  val TOk_Void = rule("TOk-Void",
    Judg(TOk, Void())
    // if ----------------
  )
  val TOk_Nat = rule("TOk-Dbl",
    Judg(TOk, Dbl())
    // if ----------------
  )
  val TOk_Bool = rule("TOk-Bool",
    Judg(TOk, Bool())
    // if ----------------
  )
  val TOk_Vec = rule("TOk-Vec",
    Judg(TOk, Vec("T"~Typ)),
    // if ----------------
    Judg(TOk, "T"~Typ)
  )
  val TOk_Arr = rule("TOk-Arr",
    Judg(TOk, App(Arr, "t1"~Typ, "t2"~Typ)),
    // if ----------------
    Judg(TOk, "t1"~Typ),
    Judg(TOk, "t2"~Typ)
  )

  val CtxOk = symbol("CtxOk", in = List(Ctx), out = Prop)
  val CtxOk_empty = rule("CtxOk-empty",
    Judg(CtxOk, App(empty))
    // if ----------------
  )
  val CtxOk_bind = rule("CtxOk-Arr",
    Judg(CtxOk, bind("C"~Ctx, "x"~Name, "T"~Typ)),
    // if ----------------
    Judg(TOk, "T"~Typ),
    Judg(CtxOk, "C"~Ctx)
  )

  val Typed = symbol("Typed", in = List(Ctx, Exp, Typ), out = Prop)
  val TypedS = symbol("TypedS", in = List(Ctx, Stm, Typ), out = Prop)

  val Typed_ref = rule("Typed-ref",
    Judg(Typed, "C"~Ctx, ref("x"~Name), "T"~Typ),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx)
  )
  val Typed_num = rule("Typed-num",
    Judg(Typed, "C"~Ctx, num("n"~Num), Dbl())
    // if ----------------
  )
  val Typed_vtrue = rule("Typed-vtrue",
    Judg(Typed, "C"~Ctx, vtrue(), Bool())
    // if ----------------
  )
  val Typed_vfalse = rule("Typed-vfalse",
    Judg(Typed, "C"~Ctx, vfalse(), Bool())
    // if ----------------
  )
  val Typed_num_binop = rule("Typed-num-binop",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, "op"~BinOp, "e2"~Exp), Dbl()),
    // if ----------------
    mkOr(Seq(
      equ(BinOp)("op"~BinOp, add()),
      equ(BinOp)("op"~BinOp, sub()),
      equ(BinOp)("op"~BinOp, mul()),
      equ(BinOp)("op"~BinOp, div())
    )).asInstanceOf[App].toJudg,
    Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
  )
  val Typed_bool_binop = rule("Typed-bool-binop",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, "op"~BinOp, "e2"~Exp), Bool()),
    // if ----------------
    Judg(OR,
      equ(BinOp)("op"~BinOp, and()),
      equ(BinOp)("op"~BinOp, or())
    ),
    Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
  )
  val Typed_num_compare = rule("Typed-num-compare",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, "op"~BinOp, "e2"~Exp), Bool()),
    // if ----------------
    mkOr(Seq(
      equ(BinOp)("op"~BinOp, eqop()),
      equ(BinOp)("op"~BinOp, lt()),
      equ(BinOp)("op"~BinOp, gt())
    )).asInstanceOf[App].toJudg,
    Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Dbl())
  )
  val Typed_bool_equals = rule("Typed-bool-equals",
    Judg(Typed, "C"~Ctx, binop("e1"~Exp, eqop(), "e2"~Exp), Bool()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Bool()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, Bool())
  )
  val Typed_neg = rule("Typed-neg",
    Judg(Typed, "C"~Ctx, unop(neg(), "e1"~Exp), Dbl()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Dbl())
  )
  val Typed_not = rule("Typed-not",
    Judg(Typed, "C"~Ctx, unop(neg(), "e1"~Exp), Bool()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Bool())
  )
  val Typed_vecnew = rule("Typed-vecalloc",
    Judg(Typed, "C"~Ctx, vecnew("size"~Exp, "T"~Typ), Vec("T"~Typ)),
    // if ----------------
    Judg(Typed, "C"~Ctx, "size"~Exp, Dbl())
  )
  val Typed_vecread = rule("Typed-vecalloc",
    Judg(Typed, "C"~Ctx, vecread("e"~Exp, "ix"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Vec("T"~Typ)),
    Judg(Typed, "C"~Ctx, "ix"~Exp, Dbl())
  )
  val Typed_block = rule("Typed-block",
    Judg(Typed, "C"~Ctx, block("s"~Stm), "T"~Typ),
    // if ----------------
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T"~Typ)
  )


  val Typed_uniqueness = rule(Lemma("Typed-uniqueness",
    Judg(equ(Typ), "T1"~Typ, "T2"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T1"~Typ),
    Judg(Typed, "C"~Ctx, "e"~Exp, "T2"~Typ)
  ))
  val Typed_TOk = rule(Lemma("Typed-TOk",
    Judg(TOk, "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  ))
  val Typed_weakening = rule(Lemma("Typed-weakening",
    Judg(Typed, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "e"~Exp, "T"~Typ),
    // if ----------------
    Judg(notin(Ctx), "x"~Name, "C"~Ctx),
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  ))
  val Typed_exchange = rule(Lemma("Typed-exchange",
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "y"~Name, "Ty"~Typ),
      "e"~Exp,
      "T"~Typ),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "y"~Name, "Ty"~Typ),
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ)
  ))
  val Typed_contraction = rule(Lemma("Typed-contraction",
    Judg(Typed,
      bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ),
    // if ----------------
    Judg(Typed,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "x"~Name, "Tx"~Typ),
      "e"~Exp,
      "T"~Typ)
  ))
  val Typed_strengthening = rule(Lemma("Typed-strengthening",
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
    // if ----------------
    Judg(notin(Exp), "x"~Name, "e"~Exp),
    Judg(Typed, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "e"~Exp, "T"~Typ)
  ))


  val TypedS_skip = rule("TypedS-skip",
    Judg(TypedS, "C"~Ctx, skip(), Void())
  )
  val TypedS_declare = rule("TypedS-declare",
    Judg(TypedS, "C"~Ctx, declare("x"~Name, "T"~Typ, "e"~Exp, "body"~Stm), "U"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ),
    Judg(TypedS, bind("C"~Ctx, "x"~Name, "T"~Typ), "body"~Stm, "U"~Typ)
  )
  val TypedS_assign = rule("TypedS-assign",
    Judg(TypedS, "C"~Ctx, assign("x"~Name, "e"~Exp), "T"~Typ),
    // if ----------------
    Judg(Lookup, "x"~Name, "T"~Typ, "C"~Ctx),
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  )
  val TypedS_seq = rule("TypedS-seq",
    Judg(TypedS, "C"~Ctx, seq("s1"~Stm, "s2"~Stm), "T"~Typ),
    // if ----------------
    Judg(TypedS, "C"~Ctx, "s1"~Stm, "U"~Typ),
    Judg(TypedS, "C"~Ctx, "s2"~Stm, "T"~Typ)
  )
  val TypedS_cond = rule("TypedS-cond",
    Judg(TypedS, "C"~Ctx, cond("e"~Exp, "then"~Stm, "else"~Stm), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Bool()),
    Judg(TypedS, "C"~Ctx, "then"~Stm, "T"~Typ),
    Judg(TypedS, "C"~Ctx, "else"~Stm, "T"~Typ)
  )
  val TypedS_loop = rule("TypedS-loop",
    Judg(TypedS, "C"~Ctx, loop("e"~Exp, "body"~Stm), Void()),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, Bool()),
    Judg(TypedS, "C"~Ctx, "body"~Stm, "T"~Typ)
  )
  val TypedS_vecwrite = rule("TypedS-verwrite",
    Judg(TypedS, "C"~Ctx, vecwrite("e1"~Exp, "ix"~Exp, "e2"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e1"~Exp, Vec("T"~Typ)),
    Judg(Typed, "C"~Ctx, "ix"~Exp, Dbl()),
    Judg(Typed, "C"~Ctx, "e2"~Exp, "T"~Typ)
  )
  val TypedS_exp = rule("TypedS-exp",
    Judg(TypedS, "C"~Ctx, exp("e"~Exp), "T"~Typ),
    // if ----------------
    Judg(Typed, "C"~Ctx, "e"~Exp, "T"~Typ)
  )


  val TypedS_uniqueness = rule(Lemma("TypedS-uniqueness",
    Judg(equ(Typ), "T1"~Typ, "T2"~Typ),
    // if ----------------
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T1"~Typ),
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T2"~Typ)
  ))
  val TypedS_TOk = rule(Lemma("TypedS-TOk",
    Judg(TOk, "T"~Typ),
    // if ----------------
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T"~Typ)
  ))
  val TypedS_weakening = rule(Lemma("TypedS-weakening",
    Judg(TypedS, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "s"~Stm, "T"~Typ),
    // if ----------------
    Judg(notin(Ctx), "x"~Name, "C"~Ctx),
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T"~Typ)
  ))
  val TypedS_exchange = rule(Lemma("TypedS-exchange",
    Judg(TypedS,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "y"~Name, "Ty"~Typ),
      "s"~Stm,
      "T"~Typ),
    // if ----------------
    Judg(neq(Name), "x"~Name, "y"~Name),
    Judg(TypedS,
      bind(bind(
        "C"~Ctx,
        "y"~Name, "Ty"~Typ),
        "x"~Name, "Tx"~Typ),
      "s"~Stm,
      "T"~Typ)
  ))
  val TypedS_contraction = rule(Lemma("TypedS-contraction",
    Judg(TypedS,
      bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
      "s"~Stm,
      "T"~Typ),
    // if ----------------
    Judg(TypedS,
      bind(bind(
        "C"~Ctx,
        "x"~Name, "Tx"~Typ),
        "x"~Name, "Tx"~Typ),
      "s"~Stm,
      "T"~Typ)
  ))
  val TypedS_strengthening = rule(Lemma("TypedS-strengthening",
    Judg(TypedS, "C"~Ctx, "s"~Stm, "T"~Typ),
    // if ----------------
    Judg(notin(Stm), "x"~Name, "s"~Stm),
    Judg(TypedS, bind("C"~Ctx, "x"~Name, "Tx"~Typ), "s"~Stm, "T"~Typ)
  ))

}