package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{FixedVars, InductionHypotheses}
import de.tu_darmstadt.veritas.backend.{Configuration, MainTrans, TypingTrans}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.smtlib.SMTLibFile
import de.tu_darmstadt.veritas.backend.tff.TffFile
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.inputdsl.FunctionDSL.FunExpFalse

import scala.util.{Failure, Success, Try}

trait TPTP extends VerifierFormat {
  override def toString: String = super.toString
}

case class FOFFormat(fof: FofFile) extends TPTP {
  override def toString: String = fof.toPrettyString()
}

case class TFFFormat(tff: TffFile) extends TPTP {
  override def toString: String = tff.toPrettyString()
}

case class SMTLibFormat(smtLib: SMTLibFile) extends VerifierFormat {
  override def toString: String = smtLib.toPrettyString()
}



case class FormatTypeInferenceError(e: TypeInference.TypeError) extends TransformerError

case class FormatTransformationError[T](e: TransformationError[T]) extends TransformerError

case class FormatBackendError[T](e: BackendError[T]) extends TransformerError

case class FormatOtherError(message: String) extends TransformerError

/**
  * Translate VeriTaS module to TFF using the "most advantageous" strategies we determined so far
  * (module transformations)
  */
class VeritasTransformer[Format](config: Configuration) extends Transformer[VeritasConstruct, VeritasConstruct, Format] {
  override def transformProblem(goal: VeritasConstruct, spec: VeritasConstruct, parentedges: Iterable[EdgeLabel], assumptions: Iterable[VeritasConstruct]): Try[Format] = {
    spec match {
      case Module(name, imps, moddefs) => {

        val goaldef: ModuleDef = goal match {
          case g: ModuleDef => g
          case _ => {
            println(s"Goal $goal was not a ModuleDef, defaulting to goal false");
            Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
          }
        }


        val assmAxioms: Iterable[Axioms] =
          for (assm <- assumptions if assm.isInstanceOf[Goals])
            yield Axioms(assm.asInstanceOf[Goals].goals)

        val propagatedInfo = (for (el <- parentedges) yield el.propagateInfoList).toSet

        if (propagatedInfo.size > 1)
          Failure(FormatOtherError(s"The given edges for transforming the proof problem " +
            s"for $goal disagreed in the propagatable information $propagatedInfo"))
        else {
          //wrap goal in local block together with info to be propagated from assumptions
          val augmentedpropagInfo: Seq[ModuleDef] =
            if (propagatedInfo.isEmpty)
              assmAxioms.toSeq ++ Seq(goaldef)
            else {
              def extractModuleDef[A](a: A): Option[ModuleDef] =
                a match {
                  case m: ModuleDef => Some(m)
                  case _ => {
                    println(s"Given propagatable info $a was not a ModuleDef, therefore ignored");
                    None
                  }
                }

              for (p <- propagatedInfo.head;
                   pi <- p.propagateInfo();
                   mpi <- extractModuleDef(pi))
                yield mpi
            }

          val augmentedgoal =
            if (augmentedpropagInfo.isEmpty)
              assmAxioms.toSeq ++ Seq(goaldef)
            else
              Seq(Local(augmentedpropagInfo ++ assmAxioms.toSeq ++ Seq(goaldef)))


          val module = Module(name + "Transformed", imps, moddefs ++ augmentedgoal)

          try {
            val mods = MainTrans(Seq(module))(config)
            val files = mods.map(m => TypingTrans.finalEncoding(m)(config))

            if (ifConfig(FinalEncoding, FinalEncoding.TFF)(config))
              Success(TFFFormat(files.head.asInstanceOf[TffFile]))
            else if (ifConfig(FinalEncoding, FinalEncoding.SMTLib)(config))
              Success(SMTLibFormat(files.head.asInstanceOf[SMTLibFile]))
            else
              Success(FOFFormat(files.head.asInstanceOf[FofFile]))

          } catch {
            case tinf: TypeInference.TypeError => Failure(FormatTypeInferenceError(tinf))
            case trans: TransformationError[_] => Failure(FormatTransformationError(trans))
            case be: BackendError[_] => Failure(FormatBackendError(be))
            case cast: ClassCastException => Failure(FormatOtherError(s"Module $module was not transformed to the expected format."))
            case e: Exception => throw e
          }
        }

      }
      case _ => Failure(FormatOtherError(s"The problem specification passed to the VeritasTransformerBestStrat was not a Module: $spec"))
    }
  }
}

object VeritasTransformerBestStrat extends VeritasTransformer(
  Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
    Simplification -> Simplification.LogicalAndConstructors,
    VariableEncoding -> VariableEncoding.InlineEverything,
    Selection -> Selection.SelectAll,
    Problem -> Problem.All)))

