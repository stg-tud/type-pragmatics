package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{FixedVars, InductionHypotheses}
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
  override def transformProblem(goal: VeritasConstruct, spec: VeritasConstruct,
                                parentedges: Iterable[EdgeLabel],
                                assumptions: Iterable[VeritasConstruct]): Try[TPTP] = {
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
          Failure(TPTPOtherError(s"The given edges for transforming the proof problem " +
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

          val transformationConfiguration = Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
            Simplification -> Simplification.LogicalAndConstructors,
            VariableEncoding -> VariableEncoding.InlineEverything,
            Selection -> Selection.SelectAll,
            Problem -> Problem.All))

          try {
            val mods = MainTrans(Seq(module))(transformationConfiguration)
            val files = mods.map(m => TypingTrans.finalEncoding(m)(transformationConfiguration))

            if (ifConfig(FinalEncoding, FinalEncoding.TFF)(transformationConfiguration))
              Success(TFFFormat(files.head.asInstanceOf[TffFile]))
            else
              Success(FOFFormat(files.head.asInstanceOf[FofFile]))
          } catch {
            case tinf: TypeInference.TypeError => Failure(TPTPTTypeInferenceError(tinf))
            case trans: TransformationError[_] => Failure(TPTPTransformationError(trans))
            case be: BackendError[_] => Failure(TPTPBackendError(be))
            case cast: ClassCastException => Failure(TPTPOtherError(s"Module $module was not transformed to the expected format."))
            case e: Exception => throw e
          }
        }

      }
      case _ => Failure(TPTPOtherError(s"The problem specification passed to the VeritasTransformerBestStrat was not a Module: $spec"))
    }
  }
}
