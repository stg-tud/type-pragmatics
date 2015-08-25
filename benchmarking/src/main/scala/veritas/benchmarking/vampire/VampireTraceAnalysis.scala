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
    for (i <- 1 to Math.min(10, newClauses.length))
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
