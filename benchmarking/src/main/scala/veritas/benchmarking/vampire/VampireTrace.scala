package veritas.benchmarking.vampire

import veritas.benchmarking.ResultDetails


trait Term {
  def subst(s: Map[String, Term]): Term
  def contains(s: Symbol): Boolean
}
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

  override def subst(s: Map[String, Term]) =
    s.get(x) match {
      case None => this
      case Some(t) => t
    }

  override def contains(s: Symbol) = s.x == x
}
case class FunApp(f: Symbol, xs: Seq[Term]) extends Term {
  override def toString = s"$f(${xs.mkString(",")})"
  override def subst(s: Map[String, Term]) = FunApp(f, xs map (_.subst(s)))
  override def contains(s: Symbol) = f.contains(s) || xs.exists(_.contains(s))
}

trait Atom {
  def subst(s: Map[String, Term]): Atom
  def contains(s: Symbol): Boolean
}
case object False extends Atom {
  override def toString = "$false"
  override def subst(s: Map[String, Term]) = False
  override def contains(s: Symbol) = false
}
case class PredApp(f: Symbol, xs: Seq[Term]) extends Atom {
  override def toString = s"$f(${xs.mkString(",")})"
  override def subst(s: Map[String, Term]) = PredApp(f, xs map (_.subst(s)))
  override def contains(s: Symbol) = f.contains(s) || xs.exists(_.contains(s))
}
case class Eq(t1: Term, t2: Term) extends Atom {
  override def toString = s"$t1 = $t2"
  override def subst(s: Map[String, Term]) = Eq(t1.subst(s), t2.subst(s))
  override def contains(s: Symbol) = t1.contains(s) || t2.contains(s)
}

trait Literal {
  def subst(s: Map[String, Term]): Literal
  def contains(s: Symbol): Boolean
}
case class Pos(a: Atom) extends Literal {
  override def toString = a.toString
  override def subst(s: Map[String, Term]) = Pos(a.subst(s))
  override def contains(s: Symbol) = a.contains(s)
}
case class Neg(a: Atom) extends Literal {
  override def toString = a match {
    case False => "$true"
    case PredApp(_, _) => "~" + a.toString
    case Eq(t1, t2) => s"$t1 != $t2"
  }
  override def subst(s: Map[String, Term]) = Neg(a.subst(s))
  override def contains(s: Symbol) = a.contains(s)
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
