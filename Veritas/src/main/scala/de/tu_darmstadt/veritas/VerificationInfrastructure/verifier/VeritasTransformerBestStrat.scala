package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{FixedVar, InductionHypothesis, StructInductCase}
import de.tu_darmstadt.veritas.backend.{Configuration, MainTrans, TypingTrans}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpFalse, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.smtlib.SMTLibFile
import de.tu_darmstadt.veritas.backend.tff.TffFile
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.backend.util.{BackendError, FreeVariables}
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
class VeritasTransformer[Format <: VerifierFormat](val config: Configuration, formatProducer: VerifierFormat => Format) extends Transformer[VeritasConstruct, VeritasConstruct, Format] {
  override def transformProblem(goal: VeritasConstruct, spec: VeritasConstruct, parentedges: Iterable[EdgeLabel], assumptions: Iterable[(EdgeLabel, VeritasConstruct)]): Try[Format] = {
    spec match {
      case Module(name, imps, moddefs) => {

        val goaldef: ModuleDef = goal match {
          case g: ModuleDef => g
          case _ => {
            println(s"Goal $goal was not a ModuleDef, defaulting to goal false");
            Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
          }
        }

        def getFixedMVs(els: Iterable[EdgeLabel]): Set[MetaVar] =
          (for (el <- els; pinf <- el.propagateInfoList
                if (pinf match {
                  case FixedVar(MetaVar(_)) => true;
                  case _ => false
                })) yield
            el.asInstanceOf[FixedVar[_]].fixedvar.asInstanceOf[MetaVar]).toSet

        def getIHs(els: Iterable[EdgeLabel]): Set[InductionHypothesis[VeritasFormula]] =
          (for (el <- els; pinf <- el.propagateInfoList
                if (pinf match {
                  case InductionHypothesis(_) => true
                  case _ => false
                })) yield
            pinf.asInstanceOf[InductionHypothesis[VeritasFormula]]).toSet


        //Step 0: Determine intersection of fixed variables and IHs from incoming and outgoing edges;
        //for the moment, assume that there are never disagreements in fixed variables of parentedges or within the edges of the children!
        //code below will simply ignore any fixed vars/IHs that the edges disagree upon!
        //also assume that fixed variables that are named equally in parentedges and childedges are meant to refer
        //to the same variables, and that fixed variables that are named differently are different from each other!
        val childedges = assumptions map (_._1)
        val childassms = assumptions map (_._2)

        val parentfvs: Set[MetaVar] = getFixedMVs(parentedges)
        val childfvs: Set[MetaVar] = getFixedMVs(childedges)

        val fvs_intersection = parentfvs intersect childfvs
        val fvs_parentsonly = parentfvs diff fvs_intersection
        val fvs_childrenonly = childfvs diff fvs_intersection

        val parentihs: Set[InductionHypothesis[VeritasFormula]] = getIHs(parentedges)
        val childihs: Set[InductionHypothesis[VeritasFormula]] = getIHs(childedges)

        //for the moment, assume that IHs will always be syntactically equivalent
        //TODO: implement alpha-equivalence on Veritas expressions?
        val ihs_intersection = parentihs intersect childihs
        val ihs_parentsonly = parentihs diff ihs_intersection
        val ihs_childrenonly = childihs diff ihs_intersection

        //Step 1: form constant definitions for the fixed variables of the parents (including the ones shared with children)
        // (requires type inference for fixed meta variable)

        //TODO new problem transformation Step 1

        //Step 2: make assumptions (Axioms) out of parentihs:
        //for ihs from ihs_parentsonly, fix all variables that appear in parentfvs
        //for ihs in ihs_intersection, fix all variables in fvs_intersection, universally quantify the rest

        //TODO new problem transformation Step 2

        //Step 3: make assumptions (Axioms) out of child goals:
        //add ihs from ihs_childrenonly as premises to childassms (encode nested implication a => b via !a \/ b)
        //fix all fixed variables from fvs_intersection, fixed variables from fvs_childrenonly have to be universally quantified

        //TODO new problem transformation Step 3

        //Step 4: fix parentfvs in goal

        //TODO new problem transformation Step 4

        //Step 5: wrap everything in a local block

        //TODO new problem transformation Step 5



        //TODO: cleanup old code, remove


        //Part 1: form assumptions - check edges to children for propagatable info such as fixed variables and IHs:
        //IHs need to be included in implication, intersection of fixed variables must not be quantified twice while doing this
        // solution: transform IHs to disjunction, add as assumptions to the axioms gained from the child obligations

        val assmAxioms: Iterable[Axioms] =
          for ((el, assm) <- assumptions) yield
            assm match {
              case Goals(ax@Seq(TypingRule(name, prems, conseqs)), _) =>
                if (el.propagateInfoList.isEmpty)
                // assumption can be transformed to axiom as is
                  Axioms(ax)
                else
                // need to parse propagatable info, look for fixed variables / IHs, and integrate into final assumption
                {
                  val ihs: Seq[InductionHypothesis[_]] =
                    (el.propagateInfoList filter {
                      case ih@InductionHypothesis(_) => true
                      case _ => false
                    }) map (_.asInstanceOf[InductionHypothesis[_]])

                  val fvs: Set[MetaVar] =
                    ((el.propagateInfoList filter {
                      case fv@FixedVar(_) => true
                      case _ => false
                    }) map {
                      case FixedVar(mv@MetaVar(_)) => mv
                      case FixedVar(FunctionMeta(mv@MetaVar(_))) => mv
                    }).toSet


                  // create additional premises for the assumptions (encoding nested implication a => b via !a \/ b)
                  // add inner universal quantification for all free variables except the fixed ones (no unsound double quantification of fixed variables!!)
                  val additionalAssms: Seq[TypingRuleJudgment] = for (ih <- ihs) yield ih.propagateInfo() match {
                    case Axioms(Seq(TypingRule(_, premises, consequences))) => {
                      val ihprems: Seq[Seq[TypingRuleJudgment]] = premises map (tr => Seq(NotJudgment(tr)))
                      val freevars_in_ih = FreeVariables.freeVariables(ihprems.flatten ++ consequences, fvs) //ignore fixed variables
                      ForallJudgment(freevars_in_ih.toSeq, Seq(OrJudgment(ihprems ++ Seq(consequences))))
                    }
                  }
                  Axioms(Seq(TypingRule(name, additionalAssms ++ prems, conseqs)))
                }

              case _ => sys.error(s"Assumption $assm in the proof graph had a format which is currently not supported.")
            }


        //Part 2: Check incoming edges (parentedges) for fixed variables/IHS. If yes, introduce constants for fixed variables
        //and transform all assumptions and the final obligation by replacing the MetaVars for the corresponding variables
        //with FunctionExpApp(...) constructs (to refer to the constants)


        val propagatedInfo = parentedges map (el => el.propagateInfoList)

        val all_same_elements = (propagatedInfo map (ps => ps.toSet)).toSeq.distinct


        //wrap goal in local block together with info to be propagated from assumptions
        val augmentedpropagInfo: Seq[ModuleDef] =
          if (propagatedInfo.isEmpty || all_same_elements.size > 1)
          //either there is no propagated info, or the edges disagree in the propagated info
          //in the later case, the only sound way is to ignore the propagatedInfo in the proof problem
          //(may introduce incompleteness
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
                 mpi <- extractModuleDef(p.propagateInfo()))
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
            Success(formatProducer(TFFFormat(files.head.asInstanceOf[TffFile])))
          else if (ifConfig(FinalEncoding, FinalEncoding.SMTLib)(config))
            Success(formatProducer(SMTLibFormat(files.head.asInstanceOf[SMTLibFile])))
          else
            Success(formatProducer(FOFFormat(files.head.asInstanceOf[FofFile])))

        } catch {
          case tinf: TypeInference.TypeError => Failure(FormatTypeInferenceError(tinf))
          case trans: TransformationError[_] => Failure(FormatTransformationError(trans))
          case be: BackendError[_] => Failure(FormatBackendError(be))
          case cast: ClassCastException => Failure(FormatOtherError(s"Module $module was not transformed to the expected format."))
          case e: Exception => throw e
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
    Problem -> Problem.All)), x => x.asInstanceOf[TFFFormat])
