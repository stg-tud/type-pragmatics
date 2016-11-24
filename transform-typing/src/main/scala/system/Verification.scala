package system

import system.Syntax._

object Verification {
  case class VerificationError(msg: String) extends Exception

  case class Language(sorts: Set[_ <: ISort], syms: Set[Symbol], rules: Set[Rule]) {
    override def toString: String = {
      s"""sorts
         |${sorts.mkString(", ")}
         |
         |symbols
         |${syms.mkString("\n")}
         |
         |rules
         |${rules.mkString("\n\n")}
       """.stripMargin
    }
  }

  trait Obligation
  case class FailedObligation(msg: String) extends Obligation
  case class ProofObligation(lang: Language, assumptions: Seq[Rule], goals: Seq[Judg]) extends Obligation
}