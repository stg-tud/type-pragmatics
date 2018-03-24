package system

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff.TffAnnotated
import system.Syntax._
import system.optimize._
import veritas.benchmarking
import veritas.benchmarking._
import veritas.benchmarking.vampire.VampireConfig

object Verification {
  case class VerificationError(msg: String) extends Exception

  case class ProofObligation(
      name: String,
      lang: Language,
      opaques: Seq[Symbol],
      existentials: Set[Var],
      axioms: Seq[Rule],
      trans: Option[Transformation],
      assumptions: Seq[Judg],
      goals: Seq[Judg],
      gensym: Gensym) {

    assert(goals.nonEmpty, s"May not construct proof obligation without any goals")

    override def toString: String = {
      val indent = "  "
      val ps = axioms.mkString("\n" + indent)
      val psn = if (ps.isEmpty) "" else "\n"
      val exists = if (existentials.isEmpty) "" else existentials.mkString(" where exists ", ",", "")
      s"""
         |Proof obligation $name in ${lang.name}:
         |
         |goals$exists
         |${goals.mkString("\n")}
         |
         |axioms
         |${axioms.mkString("\n")}
         |
         |assumptions
         |${assumptions.mkString("\n")}
       """.stripMargin
    }

    def allTrans: Seq[Transformation] = lang.transs ++ trans

    lazy val asTFF: Seq[TffAnnotated] = {
      var tff: Seq[TffAnnotated] = GenerateTFF.compileLanguage(lang)
      if (trans.isDefined)
        tff ++= GenerateTFF.compileTransformation(trans.get, lang, false)
      tff ++= opaques.map(GenerateTFF.compileSymbolDeclaration(_))
      tff ++= axioms.map(GenerateTFF.compileRuleDecl(_))

      val assumptionVars = assumptions.flatMap(_.freevars).toSet.diff(existentials)
      val goalVars = goals.flatMap(_.freevars).toSet.diff(existentials)
      val universalVars = (assumptionVars.map(GenerateTFF.compileVar(_, typed = true)) ++ goalVars.map(GenerateTFF.compileVar(_, typed = true)))
      val existentialVars = existentials.map(GenerateTFF.compileVar(_, typed = true))

      val assumptionFormula = Parenthesized(And(assumptions.map(GenerateTFF.compileJudg(_))))
      val goalFormula = Parenthesized(And(goals.map(GenerateTFF.compileJudg(_))))
      val goalBody = Parenthesized(Impl(assumptionFormula, goalFormula))
      tff :+= TffAnnotated(s"Goal-$name", Conjecture,
        ForAll(universalVars.toSeq,
          Exists(existentialVars.toSeq,
            goalBody)))

      tff
    }

    def optimized: Stream[ProofObligation] = {
      val obls1 = GoalUnpacking.unpackObligation(this).toStream
      val obls2 = obls1.flatMap(GoalNormalization.normalizeObligation(_))
      val obls3 = obls2.map(ExistentialHints.existentialHints(_))
      val obls4 = obls3.map(DropUnreachableDefinitions.dropUnreachable(_))
      obls4
    }

    def verify(mode: String = "casc", timeout: Double = 30, otherArgs: Seq[String] = Seq()): ProverResult =
      Verification.verify(this, mode, timeout, otherArgs)
  }



  val vampireConfig = new VampireConfig("4.0")
  val vampireSilentArgs = Seq("--forced_options", "proof=off")

  val runConfig = benchmarking.Main.Config(
    proverConfigs = Seq(vampireConfig),
    logExec = true,
//    logPerFile = true,
    logProof = true
//    logDisproof = true,
//    logInconclusive = true
  )

  case class ProverResult(val file: File,
                          val status: ProverStatus,
                          val timeSeconds: Option[Double],
                          val details: ResultDetails)

  def verify(p: ProofObligation, mode: String = "casc", timeout: Double = 30, otherArgs: Seq[String] = Seq()): ProverResult = {
    val tff = p.asTFF
    verify(p.name, tff, mode, timeout, otherArgs)
  }

  def verify(name: String, tff: Seq[TffAnnotated], mode: String, timeout: Double, otherArgs: Seq[String]): ProverResult = {
    val file = File.createTempFile("transform-typing", ".fof")
    new PrintWriter(file) { tff.foreach(t => write(t.toPrettyString() + "\n")); close }
    println(s"Verifying $name via TFF $file")

    val vconfig = vampireConfig.copy(mode = mode, otherArgs = otherArgs)
    val runner = new Runner(
      runConfig.copy(files = Seq(file), proverConfigs = Seq(vconfig), timeout = timeout))
    try {runner.run()}
    catch {case _: NullPointerException => }
    println()

    var summaries = runner.summary.getFileSummaries
    if (summaries.size == 1 && summaries.head._2.size == 1) {
      val result = summaries.head._2.head._2.proverResult
      if (result.status == Proved)
        println(s"SUCCESS $name\n\n")
      else if (result.status.isInstanceOf[Inconclusive] && result.details.toList.exists(_.contains("differs from tool time"))) {
        println(s"SUSPICIOUS TIMING: REPEATING VERIFICATION ATTEMPT")
        try {runner.run()}
        catch {case _: NullPointerException => }
        println()
        summaries = runner.summary.getFileSummaries
      }
      else if (result.status.isInstanceOf[Inconclusive] && result.timeSeconds.isDefined && (result.timeSeconds.get * 10 < timeout)) {
        println(s"SUSPICIOUS TIMING:  REPEATING VERIFICATION ATTEMPT")
        try {runner.run()}
        catch {case _: NullPointerException => }
        println()
        summaries = runner.summary.getFileSummaries
      }
      else
        println(s"FAILURE $name\n\n")
      ProverResult(file, result.status, result.timeSeconds, result.details)
    }
    else {
      println(s"ERROR $name\n\n")
      ProverResult(file, Inconclusive("ERROR"), None, null)
    }
  }
}