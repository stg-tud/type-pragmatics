package veritas.benchmarking.vampire

import veritas.benchmarking.{ContributedOptions, Main}
import scopt.OptionParser

object VampireTraceAnalisisOptions extends ContributedOptions {
  // shared between all vampire configurations
  val analysisSeq = collection.mutable.ListBuffer[VampireTraceAnalisis]()

  def contributeOptions(p: OptionParser[Main.Config]) = {
    p.opt[Unit]("vampire-clause-summary") action { (_,config) =>
      analysisSeq += ClauseSummary
      config
    } text("Summarizes the clauses of inconclusive prove attempts")

    p.opt[Int]("vampire-top-new") action { (k,config) =>
      analysisSeq += TopNewClauses(k)
      config
    } text("Lists the top new clauses of inconclusive prove attempts")

    p.opt[Int]("vampire-top-active") action { (k,config) =>
      analysisSeq += TopActiveClauses(k)
      config
    } text("Lists the top active clauses of inconclusive prove attempts")

    p.opt[String]("vampire-filter-symbol") action { (s,config) =>
      analysisSeq += FilterForSymbol(s)
      config
    } text("Remove clauses not featuring the given symbol")

    p.opt[Int]("vampire-filter-weight") action { (w,config) =>
      analysisSeq += FilterWeight(w)
      config
    } text("Remove clauses whose weight is larger than the given weight")

    p.opt[Unit]("vampire-merge-duplicates") action { (_,config) =>
      analysisSeq += MergeDuplicates
      config
    } text("Merge clauses that have the same term")

  }
}

trait VampireTraceAnalisis {
  def analyze(trace: VampireTrace, b: StringBuilder): VampireTrace
}

case object ClauseSummary extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var numClauses = 0
    var numActive = 0
    var numPassive = 0
    var numActivePassive = 0
    for (i <- 0 until trace.clauses.length) {
      val c = trace.clauses(i)
      if (c != null) {
        numClauses += 1
        if (c.saActive > 0 && c.saPassive > 0)
          numActivePassive += 1
        else if (c.saActive > 0)
          numActive += 1
        else if (c.saPassive > 0)
          numPassive += 1
      }
    }
    b ++= s"  Constructed $numClauses new clauses (active=$numActive, active&passive=$numActivePassive, passive=$numPassive)\n"
    trace
  }
}

case class TopNewClauses(k: Int) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    val newClauses = trace.clauses.sortBy(c => if (c == null) -1 else c.saNew)
    b ++= s"  Top $k new clauses:\n"
    for (i <- 1 to Math.min(k, newClauses.length))
      b ++= f"    ${i}:\t ${newClauses(newClauses.length - i)}\n"
    trace
  }
}

case class TopActiveClauses(k: Int) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    val newClauses = trace.clauses.sortBy(c => if (c == null) -1 else c.saActive)
    b ++= s"  Top $k active clauses:\n"
    for (i <- 1 to Math.min(k, newClauses.length))
      b ++= f"    ${i}:\t ${newClauses(newClauses.length - i)}\n"
    trace
  }
}

case class FilterForSymbol(s: String) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c => if (c == null || c.term.contains(s)) true else {deleted += 1; false})
    b ++= s"  Filtered out $deleted clauses not containing symbol $s\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case class FilterWeight(w: Int) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c => if (c == null || c.weight <= w) true else {deleted += 1; false})
    b ++= s"  Filtered out $deleted clauses with weight larger than $w\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case object MergeDuplicates extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var map = Map[String, VampireClause]()
    var oldsize = 0
    trace.clauses.map(c =>
      if (c == null) {
        // nothing
      }
      else {
        oldsize += 1
        map.get(c.term) match {
          case None => map += c.term -> c
          case Some(c2) => map += c.term -> merge(c, c2)
        }
      }
    )

    val newsize = map.size
    b ++= s"  Removed ${oldsize - newsize} duplicate clauses\n"

    val newclauses = map.values.toArray
    VampireTrace(newclauses, trace.config)
  }

  def merge(c1: VampireClause, c2: VampireClause) = VampireClause(c1.term, Math.min(c1.age, c2.age), c1.weight, c1.saNew + c2.saNew, c1.saActive + c2.saActive, c1.saPassive + c2.saPassive)
}