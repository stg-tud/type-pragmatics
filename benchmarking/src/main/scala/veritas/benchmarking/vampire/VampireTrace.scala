package veritas.benchmarking.vampire

import veritas.benchmarking.ResultDetails


trait Term
case class Symbol(x: String) extends Term {
  val isVar = x(0).isUpper
  val isConst = x(0).isLower

  val isSkolem = try {
      isConst &&
      x.length >= 3 &&
      x.startsWith("sK") &&
      {x.substring(2).toInt; true}
    } catch {case _: Throwable => false}

  override def toString = x
}
case class FunApp(f: Symbol, xs: Seq[Term]) extends Term {
  override def toString = s"$f(${xs.mkString(",")})"
}

trait Atom
case object False extends Atom {
  override def toString = "$false"
}
case class PredApp(f: Symbol, xs: Seq[Term]) extends Atom {
  override def toString = s"$f(${xs.mkString(",")})"
}
case class Eq(t1: Term, t2: Term) extends Atom {
  override def toString = s"$t1 = $t2"
}

trait Literal
case class Pos(a: Atom) extends Literal {
  override def toString = a.toString
}
case class Neg(a: Atom) extends Literal {
  override def toString = a match {
    case False => "$true"
    case PredApp(_, _) => "~" + a.toString
    case Eq(t1, t2) => s"$t1 != $t2"
  }
}

// a clause is a disjunction of literals $lits
case class VampireClause(lits: Seq[Literal], age: Int, weight: Int, var saNew: Int, var saActive: Int, var saPassive: Int) {
  override def toString = s"${lits.mkString(" | ")} (weight=$weight, new=$saNew, active=$saActive, passive=$saPassive, age=$age)"
  def term = lits.mkString(" | ")
}

case class VampireTrace(clauses: Array[VampireClause], config: VampireConfig) extends ResultDetails {
  override def toString = {
    val b = StringBuilder.newBuilder
    for (i <- 0 until clauses.length) {
      b ++= s"$i:\t"
      val c = clauses(i)
      if (c == null)
        b ++= "null"
      else
        b ++= s"new=${c.saNew}, active=${c.saActive}, passive=${c.saPassive}: ${c.term}"
      b += '\n'
    }
    b.toString
  }

  override def toHumanString = {
    val b = StringBuilder.newBuilder

    var trace = this
    for (analysis <- config.traceAnalyses)
      trace = analysis.analyze(trace, b)

    b.toString()
  }
}
