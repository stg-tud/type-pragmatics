package veritas.benchmarking.vampire

import veritas.benchmarking.ResultDetails


case class VampireClause(term: String, var saNew: Int, var saActive: Int, var saPassive: Int) {
  override def toString = s"$term (new=$saNew, active=$saActive, passive=$saPassive)"
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
