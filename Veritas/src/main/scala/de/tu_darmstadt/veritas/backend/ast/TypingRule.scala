package de.tu_darmstadt.veritas.backend.ast

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class TypingRule(name: String, premises: Seq[TypingRuleJudgment], consequences: Seq[TypingRuleJudgment]) extends VeritasFormula with PrettyPrintable {
  require(!consequences.isEmpty, "typing rule without consequences is not allowed")
  override val children = Seq(premises, consequences)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    premises foreach (writer.writeln(_))
    // NOTE make the ===== bar as long as the surrounding judgments
    val lastPremiseLength = premises.lastOption.map(_.toPrettyString.lines.map(_.length).max).getOrElse(6)
    val firstConsLength = consequences.headOption.map(_.toPrettyString.lines.map(_.length).max).getOrElse(6)
    val barLength = math.max(lastPremiseLength, firstConsLength)
    writer.writeln("=" * barLength, " ", name)
    consequences.dropRight(1) foreach (writer.writeln(_))
    consequences.lastOption foreach (writer.write(_))
  }
  
  override def toString() = s"${premises.mkString("\n")}\n=== ${name}\n${consequences.mkString("\n")}"
}

object TypingRule {
  def from(term: StrategoTerm): TypingRule = term match {
    case StrategoAppl("TypingRule",
      StrategoAppl("PremiseList", premises),
      StrategoAppl("RuleName", _, StrategoString(name)),
      StrategoAppl("ConsequenceList", consequences)) => TypingRule(name,
      unpackJudgmentCons(premises) map TypingRuleJudgment.from,
      unpackJudgmentCons(consequences) map TypingRuleJudgment.from)
    case StrategoAppl("TypingRule",
      // NOTE no premises
      StrategoAppl("RuleName", _, StrategoString(name)),
      StrategoAppl("ConsequenceList", consequences)) => TypingRule(name, Nil,
      unpackJudgmentCons(consequences) map TypingRuleJudgment.from)
    case t => throw VeritasParseError(t)
  }

  // NOTE there is a "Judgement" (BE) vs "Judgment" (AE) consistency, also in Veritas.sdf3
  // We always use Judgment (AE) in the Backend, but parsing must obviously be compatible with Veritas.sdf3
  def unpackJudgmentCons(term: StrategoTerm): Seq[StrategoTerm] = term match {
    case StrategoAppl("JudgementCons", head, rest) => head +: unpackJudgmentCons(rest)
    case elem @ StrategoAppl(_, _*)                => Seq(elem)
    case t                                         => throw VeritasParseError(t)
  }
}
