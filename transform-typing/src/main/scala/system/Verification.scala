package system

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff.TffAnnotated
import system.Syntax._
import veritas.benchmarking
import veritas.benchmarking.{Proved, ProverResult, Runner}
import veritas.benchmarking.vampire.VampireConfig

object Verification {
  case class VerificationError(msg: String) extends Exception

  case class ProofObligation(
    name: String,
    lang: Language,
    opaques: Seq[Symbol],
    existentials: Set[Var],
    axioms: Seq[Rule],
    trans: Transformation,
    assumptions: Seq[Judg],
    goals: Seq[Judg],
    gensym: Gensym) {
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

    lazy val asTFF: Seq[TffAnnotated] = {
      var tff: Seq[TffAnnotated] = GenerateTFF.compileLanguage(lang)
      tff ++= GenerateTFF.compileTransformation(trans, false)
      tff ++= opaques.map(GenerateTFF.compileSymbolDeclaration(_))
      tff ++= axioms.map(GenerateTFF.compileRuleDecl(_))

      val usedVars = (assumptions.flatMap(_.freevars) ++ goals.flatMap(_.freevars)).toSet
      val assumptionVars = assumptions.foldLeft(Set[Var]())((vars, g) => vars ++ g.freevars)
        .intersect(usedVars)
        .diff(existentials)
      val goalVars = goals.foldLeft(Set[Var]())((vars, g) => vars ++ g.freevars)
        .intersect(usedVars)
        .diff(existentials)
      val universalVars = (assumptionVars.map(GenerateTFF.compileVar(_, typed = true)) ++ goalVars.map(GenerateTFF.compileVar(_, typed = true)))
      val existentialVars = existentials.intersect(usedVars).map(GenerateTFF.compileVar(_, typed = true))

      val assumptionFormula = Parenthesized(And(assumptions.map(GenerateTFF.compileJudg(_))))
      val goalFormula = Parenthesized(And(goals.map(GenerateTFF.compileJudg(_))))
      val goalBody = Parenthesized(Impl(assumptionFormula, goalFormula))
      tff :+= TffAnnotated(s"Goal-$name", Conjecture,
        ForAll(universalVars.toSeq,
          Exists(existentialVars.toSeq,
            goalBody)))

      tff
    }
  }



  val vampireConfig = new VampireConfig("4.0")
  val runConfig = benchmarking.Main.Config(
    proverConfigs = Seq(vampireConfig),
    logExec = true,
//    logPerFile = true,
    logProof = true
//    logDisproof = true,
//    logInconclusive = true
  )

  def verify(p: ProofObligation, mode: String = "casc", timeout: Int = 30): ProverResult = {
    val tff = p.asTFF

    val file = File.createTempFile("transform-typing", ".fof")
    new PrintWriter(file) { tff.foreach(t => write(t.toPrettyString() + "\n")); close }
    println(s"Verifying ${p.name} via TFF $file")

    val runner = new Runner(
      runConfig.copy(files = Seq(file), proverConfigs = Seq(vampireConfig.copy(mode = mode)), timeout = timeout))
    runner.run()
    println()
    val summaries = runner.summary.getFileSummaries
    assert(summaries.size == 1 && summaries.head._2.size == 1)

    val result = summaries.head._2.head._2.proverResult
    if (result.status == Proved)
      println(s"SUCCESS ${p.name}")
    else
      println(s"FAILURE ${p.name}")
    result
  }
}