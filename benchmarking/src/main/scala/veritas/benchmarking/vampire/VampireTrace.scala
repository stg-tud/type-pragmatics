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

  override def hashCode = 17 * (t1.hashCode + t2.hashCode)
  override def equals(o: Any) = o.isInstanceOf[Eq] && {
    val eq = o.asInstanceOf[Eq]
    t1 == eq.t1 && t2 == eq.t2 || t1 == eq.t2 && t2 == eq.t1
  }
}

trait Literal {
  def subst(s: Map[String, Term]): Literal
  def contains(s: Symbol): Boolean
  def negate: Literal
}
case class Pos(a: Atom) extends Literal {
  override def toString = a.toString
  override def subst(s: Map[String, Term]) = Pos(a.subst(s))
  override def contains(s: Symbol) = a.contains(s)
  override def negate = Neg(a)
}
case class Neg(a: Atom) extends Literal {
  override def toString = a match {
    case False => "$true"
    case PredApp(_, _) => "~" + a.toString
    case Eq(t1, t2) => s"$t1 != $t2"
  }
  override def subst(s: Map[String, Term]) = Neg(a.subst(s))
  override def contains(s: Symbol) = a.contains(s)
  override def negate = Pos(a)
}

// a clause is a disjunction of literals $lits
case class VampireClause(lits: Seq[Literal],
                         age: Int,
                         weight: Int,
                         var saNew: Int,
                         var saActive: Int,
                         var saPassive: Int,
                         ids: Seq[Int], // alternative original IDs for this clause
                         steps: Seq[VampireStep], // alternative steps that lead to this clause
                         contextConditions: Seq[VampireClause]) {
  if (lits.isEmpty)
    throw new IllegalArgumentException(s"A clause cannot be empty")

  val IDstring =
    if (VampireTraceAnalisisOptions.logIDs)
      s", ids=${ids.toList.sorted.mkString("{", ",", "}")})"
    else
      ""
  override def toString = s"${lits.mkString(" | ")} (weight=$weight, new=$saNew, active=$saActive, passive=$saPassive, age=$age$IDstring, satContext=${contextConditions.map(_.ids.head).mkString("{",",","}")})"
  def term = lits.mkString(" | ")
}

case class VampireStep(rule: String, predecessors: Seq[Int])

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
    for (analysis <- VampireTraceAnalisisOptions.analysisSeq)
      trace = analysis.analyze(trace, b)

    b.toString()
  }

  def analyze(analyses: Iterable[VampireTraceAnalisis]): (VampireTrace, String) = {
    var trace = this
    var stop = false

    val b = StringBuilder.newBuilder
    for (analysis <- analyses if !stop) {
      if (analysis == MergeTraces)
        stop = true
      trace = analysis.analyze(trace, b)
    }

    (trace, b.toString)
  }

}

case class VampireManyTraces(traces: Seq[VampireTrace]) extends ResultDetails {
  override def toString = traces.tail.foldLeft(traces.head.toString)((s, t) => s + "\n" + t.toString)

  override def toHumanString = {
    val (simplifiedTraces, descriptions) = traces.map(_.analyze(VampireTraceAnalisisOptions.analysisSeq)).unzip
    if (!VampireTraceAnalisisOptions.analysisSeq.contains(MergeTraces))
      descriptions.mkString("\n")
    else {
      val pref = descriptions.mkString("\n") + "\n"
      val (mergedTrace, mergeDesc) = MergeTraces.merge(simplifiedTraces)
      val remainingAnalyses = VampireTraceAnalisisOptions.analysisSeq.dropWhile(_ != MergeTraces).tail
      val (trace, desc) = mergedTrace.analyze(remainingAnalyses)
      pref + mergeDesc + desc
    }
  }
}