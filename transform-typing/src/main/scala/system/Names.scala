package system

import system.Syntax._
import system.optimize.GoalUnpacking

object Names {

  def fresh(s: ISort) = Symbol(s"fresh$s", in = List(s), out = Name, false)
  def notin(s: ISort) = Symbol(s"notin$s", in = List(Name, s), out = Prop)

  def freshNotin(s: ISort) = {
    val freshSym = Names.fresh(s)
    val notinSym = Names.notin(s)
    val judg = Judg(notinSym, freshSym(Var("x", s)), Var("x", s))
    Rule(s"$freshSym-$notinSym", judg)
  }

  def mkFreshJudgs(Ctx: ISort, init: Term, bind: Symbol, Typed: Symbol, ref: Symbol)
                  (names: List[(String, Term)]): Seq[Judg] = {
    var judgs = Seq[Judg]()
    var current = init

    for ((name, typ) <- names) {
      judgs +:= Judg(equ(Name), name~Name, fresh(Ctx)(current))
      current = bind(current, name~Name, typ)
    }

    // because each name is fresh in the surrounding context
//    for (i <- 0 until names.size; j <- i+1 until names.size) {
//      judgs +:= Judg(neq(Name), names(i)._1~Name, names(j)._1~Name)
//    }

    judgs.reverse
  }
}