package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.backend.{Configuration, MainTrans, TypingTrans}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.tff.TffFile
import de.tu_darmstadt.veritas.inputdsl.FunctionDSL.FunExpFalse

trait TPTP extends VerifierFormat {
  override def toString: String = super.toString
}

case class FOFFormat(fof: FofFile) extends TPTP {
  override def toString: String = fof.toPrettyString()
}

case class TFFFormat(tff: TffFile) extends TPTP {
  override def toString: String = tff.toPrettyString()
}

/**
  * Translate VeriTaS module to TFF using the "most advantageous" strategies we determined so far
  * (module transformations)
  */
class VeritasTransformerBestStrat extends Transformer[Seq[VeritasConstruct], VeritasConstruct, TPTP]{
  override def transformProblem(spec: Seq[VeritasConstruct], goal: VeritasConstruct): TPTP = {
    val moddefs: Seq[ModuleDef] =
      spec flatMap {s => s match {
        case m: ModuleDef => Seq(m)
        case _ => {println(s"Ignored $s since it was not a ModuleDef."); Seq()}
      }
    }

    val goaldef: ModuleDef = goal match {
      case g: ModuleDef => g
      case _ => {println(s"Goal $goal was not a ModuleDef, defaulting to goal false");
        Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)}
    }

    val module = Module("ProblemModule", Seq(), moddefs ++ Seq(goaldef))

    val transformationConfiguration = Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
      Simplification -> Simplification.LogicalAndConstructors,
      VariableEncoding -> VariableEncoding.InlineEverything,
      Selection -> Selection.SelectAll,
      Problem -> Problem.All))

    val mods = MainTrans(Seq(module))(transformationConfiguration)
    val files = mods.map(m => TypingTrans.finalEncoding(m)(transformationConfiguration))

    //TODO better error handling
    if (ifConfig(FinalEncoding, FinalEncoding.TFF)(transformationConfiguration))
      TFFFormat(files.head.asInstanceOf[TffFile])
    else if (ifConfig(FinalEncoding, FinalEncoding.BareFOF)(transformationConfiguration))
      FOFFormat(files.head.asInstanceOf[FofFile])
    else if (ifConfig(FinalEncoding, FinalEncoding.GuardedFOF)(transformationConfiguration))
      FOFFormat(files.head.asInstanceOf[FofFile])
    else sys.error("could not transform file!")

  }
}
