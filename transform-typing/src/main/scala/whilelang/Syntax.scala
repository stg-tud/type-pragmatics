package whilelang

import system.Syntax._

object Syntax {
  // sorts
  sort(Name)
  sort(Prop)
  val Num = sort(Sort("Num", abstractEnum = true))
  val BinOp = sort("BinOp")
  val UnOp = sort("UnOp")
  val Typ = sort("Typ")
  val Ctx = sort("Ctx")
  val Exp = sort("Exp")
  val Stm = sort("Stm")

  // constants
  val zero = symbol(Symbol("zero", in = List(), out = Num, constr = false))
  val one = symbol(Symbol("one", in = List(), out = Num, constr = false))

  // types
  val Void = symbol("Void", in = List(), out = Typ)
  val Dbl = symbol("Dbl", in = List(), out = Typ)
  val Bool = symbol("Bool", in = List(), out = Typ)
  val Vec = symbol("Vec", in = List(Typ), out = Typ)
  val Arr = symbol("Arr", in = List(Typ, Typ), out = Typ)

  // contexts
  val empty = symbol("empty", in = List(), out = Ctx)
  val bind = symbol("bind", in = List(Ctx, Name, Typ), out = Ctx)

  // binary ops
  val add = symbol("add", in = List(), out = BinOp)
  val sub = symbol("sub", in = List(), out = BinOp)
  val mul = symbol("mul", in = List(), out = BinOp)
  val div = symbol("div", in = List(), out = BinOp)
  val eqop = symbol("eqop", in = List(), out = BinOp)
  val lt = symbol("lt", in = List(), out = BinOp)
  val gt = symbol("gt", in = List(), out = BinOp)
  val and = symbol("and", in = List(), out = BinOp)
  val or = symbol("or", in = List(), out = BinOp)

  // unary opes
  val neg = symbol("neg", in = List(), out = UnOp)
  val not = symbol("not", in = List(), out = UnOp)

  // expressions
  val vtrue = symbol("vtrue", in = List(), out = Exp)
  val vfalse = symbol("vfalse", in = List(), out = Exp)
  val ref = symbol("ref", in = List(Name), out = Exp)
  val num = symbol("num", in = List(Num), out = Exp)
  val binop = symbol("binop", in = List(Exp, BinOp, Exp), out = Exp)
  val unop = symbol("unop", in = List(UnOp, Exp), out = Exp)
  val vecnew = symbol("vecnew", in = List(Exp, Typ), out = Exp)
  val vecread = symbol("vecread", in = List(Exp, Exp), out = Exp)
  val veclength = symbol("veclength", in = List(Exp), out = Exp)
  val block = symbol("block", in = List(Stm), out = Exp)

  // statements
  val skip = symbol("skip", in = List(), out = Stm)
  val declare = symbol("declare", in = List(Name, Typ, Exp, Stm), out = Stm)
  val assign = symbol("assign", in = List(Name, Exp), out = Stm)
  val seq = symbol("seq", in = List(Stm, Stm), out = Stm)
  val cond = symbol("cond", in = List(Exp, Stm, Stm), out = Stm)
  val loop = symbol("loop", in = List(Exp, Stm), out = Stm)
  val vecwrite = symbol("vecwrite", in = List(Exp, Exp, Exp), out = Stm)
  val exp = symbol("exp", in = List(Exp), out = Stm)
}
