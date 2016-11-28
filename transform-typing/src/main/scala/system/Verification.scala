package system

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.fof.{And, Conjecture}
import de.tu_darmstadt.veritas.backend.tff.TffAnnotated
import system.Syntax._
import veritas.benchmarking
import veritas.benchmarking.Runner
import veritas.benchmarking.vampire.VampireConfig

object Verification {
  case class VerificationError(msg: String) extends Exception

  trait Obligation

  case class FailedObligation(msg: String) extends Obligation

  case class ProofObligation(lang: Language, opaques: Seq[Symbol], assumptions: Seq[Rule], goals: Seq[Judg]) extends Obligation {
    override def toString: String = {
      val indent = "  "
      val ps = assumptions.mkString("\n" + indent)
      val psn = if (ps.isEmpty) "" else "\n"
      s"""
         |Proof obligation in ${lang.name}:
         |
         |goals
         |${goals.mkString("\n")}
         |
         |assumptions
         |${assumptions.mkString("\n")}
       """.stripMargin
    }

    lazy val asTFF: Seq[TffAnnotated] = {
      var tff: Seq[TffAnnotated] = GenerateTFF.compileLanguage(lang)
      tff ++= opaques.map(GenerateTFF.compileSymbolDeclaration(_))
      tff ++= assumptions.map(GenerateTFF.compileRuleDecl(_))
      tff :+= TffAnnotated("Goal", Conjecture, And(goals.map(GenerateTFF.compileJudg(_))))
      tff
    }
  }



  val vampireConfig = new VampireConfig(null)
  val runConfig = benchmarking.Main.Config(
    proverConfigs = Seq(vampireConfig),
    fullLogs = true,
    logExec = false,
    logPerFile = true,
    logProof = true,
    logDisproof = true,
    logInconclusive = true,
    logSummary = true
  )

  def verify(obl: Obligation): Unit = obl match {
    case p: ProofObligation =>
      val tff = p.asTFF

      val file = File.createTempFile("transform-typing", ".fof")
      new PrintWriter(file) { tff.foreach(t => write(t.toPrettyString() + "\n")); close }
      println(s"Wrote TFF goal to $file")

      val runner = new Runner(runConfig.copy(files = Seq(file)))
      runner.run()

    case FailedObligation(msg) => ???
  }
}