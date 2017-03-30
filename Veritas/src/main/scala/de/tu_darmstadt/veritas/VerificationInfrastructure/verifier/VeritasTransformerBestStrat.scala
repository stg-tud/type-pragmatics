package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.backend.{Configuration, MainTrans, TypingTrans}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.tff.TffFile
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.backend.util.BackendError
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


trait TPTPTransformerError extends TransformerError
case class TPTPTTypeInferenceError(e: TypeInference.TypeError) extends TPTPTransformerError
case class TPTPTransformationError[T](e: TransformationError[T]) extends TPTPTransformerError
case class TPTPBackendError[T](e: BackendError[T]) extends TPTPTransformerError
case class TPTPOtherError(message: String) extends TPTPTransformerError

/**
  * Translate VeriTaS module to TFF using the "most advantageous" strategies we determined so far
  * (module transformations)
  */
class VeritasTransformerBestStrat extends Transformer[VeritasConstruct, VeritasConstruct, TPTP] {
  override def transformProblem(spec: VeritasConstruct, goal: VeritasConstruct): Either[TPTP, TPTPTransformerError] = {
    spec match {
      case Module(name, imps, moddefs) => {

        val goaldef: ModuleDef = goal match {
          case g: ModuleDef => g
          case _ => {
            println(s"Goal $goal was not a ModuleDef, defaulting to goal false");
            Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
          }
        }

        val module = Module(name + "WithGoal", imps, moddefs ++ Seq(goaldef))

        val transformationConfiguration = Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
          Simplification -> Simplification.LogicalAndConstructors,
          VariableEncoding -> VariableEncoding.InlineEverything,
          Selection -> Selection.SelectAll,
          Problem -> Problem.All))

        try {
          val mods = MainTrans(Seq(module))(transformationConfiguration)
          val files = mods.map(m => TypingTrans.finalEncoding(m)(transformationConfiguration))

          if (ifConfig(FinalEncoding, FinalEncoding.TFF)(transformationConfiguration))
            Left(TFFFormat(files.head.asInstanceOf[TffFile]))
          else
            Left(FOFFormat(files.head.asInstanceOf[FofFile]))
        } catch {
          case tinf: TypeInference.TypeError => Right(TPTPTTypeInferenceError(tinf))
          case trans: TransformationError[_] =>  Right(TPTPTransformationError(trans))
          case be: BackendError[_] => Right(TPTPBackendError(be))
          case cast: ClassCastException => Right(TPTPOtherError(s"Module $module was not transformed to the expected format."))
          case e: Exception => throw e
        }

      }
      case _ => Right(TPTPOtherError(s"The problem specification passed to the VeritasTransformerBestStrat was not a Module: $spec"))
    }
  }
}
