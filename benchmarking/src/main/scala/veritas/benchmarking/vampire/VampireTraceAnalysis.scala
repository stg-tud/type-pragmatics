package veritas.benchmarking.vampire

import veritas.benchmarking.{ContributedOptions, Main}
import scopt.OptionParser

object VampireTraceAnalisisOptions extends ContributedOptions {
  // shared between all vampire configurations
  val analysisSeq = collection.mutable.ListBuffer[VampireTraceAnalisis]()

  var logIDs = false

  def contributeOptions(p: OptionParser[Main.Config]) = {
    p.opt[Unit]("vampire-clause-summary") unbounded() action { (_,config) =>
      analysisSeq += ClauseSummary
      config
    } text("Summarizes the clauses of inconclusive prove attempts")

    p.opt[Int]("vampire-top-weight") unbounded() action { (k,config) =>
      analysisSeq += TopWeightClauses(k)
      config
    } text("Lists the clauses with least weight")

    p.opt[Int]("vampire-top-new") unbounded() action { (k,config) =>
      analysisSeq += TopNewClauses(k)
      config
    } text("Lists the top new clauses of inconclusive prove attempts")

    p.opt[Int]("vampire-top-active") unbounded() action { (k,config) =>
      analysisSeq += TopActiveClauses(k)
      config
    } text("Lists the top active clauses of inconclusive prove attempts")

    p.opt[String]("vampire-filter-symbol") unbounded() action { (s,config) =>
      analysisSeq += FilterForSymbol(s)
      config
    } text("Remove clauses not featuring the given symbol")

    p.opt[Int]("vampire-filter-weight") unbounded() action { (w,config) =>
      analysisSeq += FilterWeight(w)
      config
    } text("Remove clauses whose weight is larger than the given weight")

    p.opt[Unit]("vampire-filter-used") unbounded() action { (_,config) =>
      analysisSeq += FilterUsed
      config
    } text("Remove clauses used for deriving other clauses")

    p.opt[Unit]("vampire-filter-tautologies") unbounded() action { (_,config) =>
      analysisSeq += FilterTautologies
      config
    } text("Remove clauses that are tautologies")

    p.opt[Unit]("vampire-filter-conditionals") unbounded() action { (_,config) =>
      analysisSeq += FilterConditionalClauses
      config
    } text("Remove clauses that are conditional under a sat-splitting context")

    p.opt[Unit]("vampire-merge-duplicates") unbounded() action { (_,config) =>
      analysisSeq += MergeDuplicates
      config
    } text("Merge identical clauses")

    p.opt[Unit]("vampire-unique-names") unbounded() action { (_,config) =>
      analysisSeq += UniqueNames
      config
    } text("Rename variables in clauses such that the naming is unique (makes clauses equal that previously only were equal modulo renaming)")

    p.opt[Unit]("vampire-simplify-inlining") unbounded() action { (_,config) =>
      analysisSeq += SimplifyWithInlining
      config
    } text("Simplify clauses through inlining of equations")

    p.opt[Unit]("vampire-simplify-facts") unbounded() action { (_,config) =>
      analysisSeq += SimplifyFacts
      config
    } text("Simplify clauses by removing negated facts")

    p.opt[Unit]("vampire-log-ids") action { (_,config) =>
      logIDs = true
      config
    } text("Show origin ids of clauses")

    p.opt[Unit]("vampire-merge-traces") action { (_,config) =>
      analysisSeq += MergeTraces
      config
    } text("Merge traces of all strategies run by Vampire")
  }
}

trait VampireTraceAnalisis {
  def analyze(trace: VampireTrace, b: StringBuilder): VampireTrace
}

case object MergeTraces extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = trace

  def merge(traces: Seq[VampireTrace]): (VampireTrace, String) = {
    var clauses = Map[Seq[Literal], VampireClause]()

    for (trace <- traces;
         clause <- trace.clauses if clause != null) {
      clauses.get(clause.lits) match {
        case None => clauses += clause.lits -> clause
        case Some(c) => clauses += clause.lits -> MergeDuplicates.merge(c, clause)
      }
    }

    (VampireTrace(clauses.values.toArray, traces.head.config), "Merging traces\n")
  }
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
        if (c.saActive > 0)
          numActive += 1
        if (c.saPassive > 0)
          numPassive += 1
      }
    }
    b ++= s"  Constructed $numClauses new clauses (active=$numActive, active&passive=$numActivePassive, passive=$numPassive)\n"
    trace
  }
}

case class TopWeightClauses(k: Int) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    val newClauses = trace.clauses.sortBy(c => if (c == null) Int.MaxValue else c.weight)
    b ++= s"  Top $k weight clauses (with least weight):\n"
    for (i <- 1 to Math.min(k, newClauses.length))
      b ++= f"    ${i}:\t ${newClauses(i-1)}\n"
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
    b ++= s"  Filtered out $deleted of ${trace.clauses.size} clauses not containing symbol $s\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case class FilterWeight(w: Int) extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c => if (c == null || c.weight <= w) true else {deleted += 1; false})
    b ++= s"  Filtered out $deleted of ${trace.clauses.size} clauses with weight larger than $w\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case object FilterUsed extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var used = Set[Int]()
    trace.clauses foreach (c =>
      if (c != null)
        c.steps foreach (used ++= _.predecessors)
    )
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c =>
      if (c == null || c.ids.exists(used.contains(_)))
        true
      else  {
        deleted += 1
        false
      }
    )
    b ++= s"  Filtered out $deleted of ${trace.clauses.size} clauses that were used for deriving other clauses\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case object FilterTautologies extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c =>
      if (c == null || !c.lits.exists(l => c.lits.contains(l.negate)))
        true
      else  {
        deleted += 1
        false
      }
    )
    b ++= s"  Filtered out $deleted of ${trace.clauses.size} clauses that were tautologies\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case object FilterConditionalClauses extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var deleted = 0
    val filteredClauses = trace.clauses.filter(c =>
      if (c == null || c.contextConditions.isEmpty)
        true
      else  {
        deleted += 1
        false
      }
    )
    b ++= s"  Filtered out $deleted of ${trace.clauses.size} clauses that are conditional under a sat-splitting context\n"
    VampireTrace(filteredClauses, trace.config)
  }
}

case object MergeDuplicates extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var map = Map[(Seq[Literal], Seq[VampireClause]), VampireClause]()
    var oldsize = 0
    trace.clauses.map(c =>
      if (c == null) {
        // nothing
      }
      else {
        oldsize += 1
        val key = (c.lits, c.contextConditions)
        map.get(key) match {
          case None => map += key -> c
          case Some(c2) => map += key -> merge(c, c2)
        }
      }
    )

    val newsize = map.size
    b ++= s"  Merged ${oldsize - newsize} of ${trace.clauses.size} duplicate clauses\n"

    val newclauses = map.values.toArray
    VampireTrace(newclauses, trace.config)
  }

  def merge(c1: VampireClause, c2: VampireClause) =
    VampireClause(
      c1.lits,
      Math.min(c1.age, c2.age),
      Math.min(c1.weight, c2.weight),
      c1.saNew + c2.saNew,
      c1.saActive + c2.saActive,
      c1.saPassive + c2.saPassive,
      c1.ids ++ c2.ids,
      c1.steps ++ c2.steps,
      c1.contextConditions)
}

case object UniqueNames extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var renamed = 0
    val newclauses = trace.clauses.map(c =>
      if (c == null)
        c
      else {
        reset()
        val (lits, changed) = c.lits.map(uniqueNames(_)).unzip
        if (changed.nonEmpty && changed.reduce(_||_))
          renamed += 1
        VampireClause(lits, c.age, c.weight, c.saNew, c.saActive, c.saPassive, c.ids, c.steps, c.contextConditions)
      }
    )

    b ++= s"  Renamed variables in $renamed clauses\n"
    VampireTrace(newclauses, trace.config)
  }

  var subst = Map[String, String]()
  var varCount = 0
  var skoVarCount = 0
  def reset(): Unit = {
    subst = Map()
    varCount = 0
    skoVarCount = 0
  }

  def uniqueNames(l: Literal): (Literal, Boolean) = l match {
    case Pos(at) =>
      val (newat, changed) = uniqueNames(at)
      (Pos(newat), changed)
    case Neg(at) =>
      val (newat, changed) = uniqueNames(at)
      (Neg(newat), changed)
  }

  def uniqueNames(at: Atom): (Atom, Boolean) = at match {
    case False => (False, false)
    case Eq(t1, t2) =>
      val (newt1, changed1) = uniqueNames(t1)
      val (newt2, changed2) = uniqueNames(t2)
      (Eq(newt1, newt2), changed1 || changed2)
    case PredApp(f, xs) =>
      val (fnew, fchanged) = uniqueNames(f)
      val (xsnew, xschanged) = xs.map(uniqueNames(_)).unzip
      (PredApp(fnew, xsnew), fchanged || xschanged.nonEmpty && xschanged.reduce(_||_))
  }

  def uniqueNames(s: Symbol): (Symbol, Boolean) =
    if (s.isConst && !s.isSkolem)
      (s, false)
    else if (subst.isDefinedAt(s.x))
      (Symbol(subst(s.x)), true)
    else if (s.isVar) {
      val nextVar = s"X$varCount"
      if (nextVar == s.x)
        (s, false)
      else {
        varCount += 1
        subst += s.x -> nextVar
        (Symbol(nextVar), true)
      }
    }
    else { // s.isSkolem
      val nextSkoVar = s"sK$skoVarCount"
      if (nextSkoVar == s.x)
        (s, false)
      else {
        skoVarCount += 1
        subst += s.x -> nextSkoVar
        (Symbol(nextSkoVar), true)
      }
    }
    

  def uniqueNames(t: Term): (Term, Boolean) = t match {
    case s: Symbol => uniqueNames(s)
    case FunApp(f, xs) =>
      val (fnew, fchanged) = uniqueNames(f)
      val (xsnew, xschanged) = xs.map(uniqueNames(_)).unzip
      (FunApp(fnew, xsnew), fchanged || xschanged.nonEmpty && xschanged.reduce(_||_))
  }
}

case object SimplifyWithInlining extends VampireTraceAnalisis {

  override def analyze(trace: VampireTrace, b: StringBuilder): VampireTrace = {
    var simplified = 0
    val newclauses = trace.clauses.map(c =>
      if (c == null)
        c
      else {
        subst = Map[String, Term]()
        var lastSize = c.lits.size
        var lits = simplify(c.lits)
        while (lastSize != lits.size) {
          lastSize = lits.size
          lits = lits map (_.subst(subst))
          lits = simplify(lits)
        }
        if (lits.size != c.lits.size) {
          simplified += 1
          lits = lits map (_.subst(subst))
          lits = lits.distinct
        }
        if (lits.isEmpty)
          null
        else
          VampireClause(lits, c.age, c.weight, c.saNew, c.saActive, c.saPassive, c.ids, c.steps, c.contextConditions)
      }
    )

    // TODO possible to eliminate all equations without recursion?
    val newtrace = VampireTrace(newclauses, trace.config)
    if (simplified > 0) {
      b ++= s"  Simplified $simplified clauses by equation inlining\n"
      analyze(newtrace, b)
    }
    else
      newtrace
  }

  var subst = Map[String, Term]()

  def simplify(lits: Seq[Literal], previous: Seq[Literal] = Seq()): Seq[Literal] =
    if (lits.isEmpty)
      previous
    else
      simplify(lits.head, lits.tail, previous)

  def simplify(current: Literal, rest: Seq[Literal], previous: Seq[Literal]): Seq[Literal] = {
    def ok(s: Symbol, t: Term) = {
      val cond1 = !subst.isDefinedAt(s.x) && !t.contains(s)
      if (s.isVar)
        cond1
      else if (s.isConst)
        cond1 && (previous.exists(_.contains(s)) || rest.exists(_.contains(s)))
      else
        false
    }

    val (s,t) = current match {
      case Neg(Eq(s:Symbol, t)) if ok(s,t) => s -> t
      case Neg(Eq(t, s:Symbol)) if ok(s,t) => s -> t
      case _ => return simplify(rest, previous :+ current)
    }

    subst += s.x -> t
    simplify(rest, previous)
  }
}

case object SimplifyFacts extends VampireTraceAnalisis {
  override def analyze(trace: VampireTrace, b: StringBuilder) = {
    var facts = Set[Literal]()
    trace.clauses.foreach(c =>
      if (c != null && c.lits.size == 1 && c.contextConditions.isEmpty)
        facts += c.lits.head
    )

    var simplified = 0
    val simplifiedClauses = trace.clauses.map(c =>
      if (c == null)
        c
      else {
        val newlits = c.lits.filter(l => !facts.contains(l.negate))
        if (newlits.size != c.lits.size)
          simplified += 1
        if (newlits.isEmpty)
          null
        else
          VampireClause(newlits, c.age, c.weight, c.saNew, c.saActive, c.saPassive, c.ids, c.steps, c.contextConditions)
      }
    )
    b ++= s"  Simplified $simplified of ${trace.clauses.size} by eliminating negated facts\n"
    VampireTrace(simplifiedClauses, trace.config)
  }
}
