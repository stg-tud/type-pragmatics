package system

import system.Syntax._

object Verification {
  case class VerificationError(msg: String) extends Exception

  case class Language(name: String, sorts: Set[_ <: ISort], syms: Set[Symbol], rules: Set[Rule]) {
    override def toString: String = {
      s"""sorts
         |${sorts.mkString(", ")}
         |
         |symbols
         |${syms.map(_.sigString).mkString("\n")}
         |
         |rules
         |${rules.mkString("\n\n")}
       """.stripMargin
    }
  }

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
  }
}