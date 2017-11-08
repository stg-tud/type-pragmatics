package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{FixedVar, InductionHypothesis, StructInductCase}
import de.tu_darmstadt.veritas.backend.{Configuration, MainTrans, TypingTrans}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.smtlib.SMTLibFile
import de.tu_darmstadt.veritas.backend.tff.TffFile
import de.tu_darmstadt.veritas.backend.transformation.{ModuleTransformation, TransformationError}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass, TypeInference}
import de.tu_darmstadt.veritas.backend.transformation.defs.TranslateTypingJudgments
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

  //for inferring types of functions and datatypes
  private val tdcollector: CollectTypesDefs = new CollectTypesDefsClass with Serializable

  //object for inferring the signature of the typing judgments over the entire spec
  private object TypingJudgmentTranslator extends TranslateTypingJudgments with Serializable {

    //convenience method for being able to apply the translator directly to a typing rule
    def apply(tr: TypingRule): TypingRule = {
      this.transTypingRules(tr).head
    }
  }

  private val defconfig = Configuration(Map(
    Simplification -> Simplification.None,
    VariableEncoding -> VariableEncoding.Unchanged,
    FinalEncoding -> FinalEncoding.TFF,
    Selection -> Selection.SelectAll,
    Problem -> Problem.All))

  // try to retrieve a TypingRule construct from a given VeritasFormula
  private def retrieveTypingRule(f: VeritasFormula): Option[TypingRule] = f match {
    case t@TypingRule(_, _, _) => Some(t)
    case tj: TypingRuleJudgment => Some(TypingRule("wrappedTypingRuleJdgm", Seq(), Seq(tj)))
    // above covers all cases such as OrJudgment, NotJudgment, ForallJudgment...
    case Goals(Seq(t@TypingRule(_, _, _)), _) => Some(t)
    case GoalsWithStrategy(_, Seq(t@TypingRule(_, _, _)), _) => Some(t)
    case Lemmas(Seq(t@TypingRule(_, _, _)), _) => Some(t)
    case LemmasWithStrategy(_, Seq(t@TypingRule(_, _, _)), _) => Some(t)
    case Axioms(Seq(t@TypingRule(_, _, _))) => Some(t)
    case _ => None
  }

  //generate a top-down traversal starting from the type of a given VeritasConstruct, based on ModuleTransformation
  private class VeritasConstructTraverser extends ModuleTransformation with Serializable {

    //subclasses can use this variable to collect the special Veritas constructs that they want to extract
    var collected: Seq[VeritasConstruct] = Seq()

    def apply(vc: VeritasConstruct): Seq[VeritasConstruct] = {
      vc match {
        case Goals(Seq(tr), _) => transTypingRules(tr)
        case GoalsWithStrategy(_, Seq(tr), _) => transTypingRules(tr)
        case Lemmas(Seq(tr), _) => transTypingRules(tr)
        case LemmasWithStrategy(_, Seq(tr), _) => transTypingRules(tr)
        case Axioms(Seq(tr)) => transTypingRules(tr)
        case m: Module => trans(m)
        case md: ModuleDef => transModuleDefs(md)
        case fd: FunctionDef => transFunctionDefs(fd)
        case fs: FunctionSig => Seq(transFunctionSig(fs))
        case feq: FunctionEq => transFunctionEqs(feq)
        case fp: FunctionPattern => transFunctionPatterns(fp)
        case tp: TypingRule => transTypingRules(tp)
        case trj: TypingRuleJudgment => transTypingRuleJudgments(trj)
        case mv: MetaVar => transMetaVars(mv)
        case fe: FunctionExp => transFunctionExps(fe)
        case fem: FunctionExpMeta => transFunctionExpMetas(fem)
        case sd: SortDef => transSortDefs(sd)
        case c: ConstDecl => transConstDecl(c)
        case dtc: DataTypeConstructor => transDataTypeConstructor(dtc, tdcollector.constrTypes(dtc.name)._2.name)
        case sr: SortRef => transSortRefs(sr)
        case _ => sys.error("Given Veritas construct not suppported by VeritasConstructTraverser.")
      }
    }
  }

  private def fixMVsinVC[C <: VeritasConstruct](vc: C, mvs_to_fix: Set[MetaVar]): C = {
    val traverser = new VeritasConstructTraverser {
      override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
        withSuper(super.transFunctionExpMeta(f)) {
          case FunctionMeta(mv@MetaVar(name)) =>
            if (mvs_to_fix contains mv)
              FunctionExpApp(name, Seq())
            else
              super.transFunctionExpMeta(f)
        }

      override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] = Seq(transFunctionExpMeta(f))
    }

    traverser(vc).head.asInstanceOf[C] //TODO catch some potential errors here if necessary
  }

  //extract fixed variables from edge labels
  private def getFixedMVs(els: Iterable[EdgeLabel]): Set[MetaVar] =
    (for (el <- els; pinf <- el.propagateInfoList
          if (pinf match {
            case FixedVar(FunctionMeta(MetaVar(_))) => true;
            case _ => false
          })) yield
    //TODO do safer casting here
      pinf.asInstanceOf[FixedVar[_]].fixedvar.asInstanceOf[FunctionMeta].metavar).toSet

  //extract induction hypotheses from edge labels
  private def getIHs(els: Iterable[EdgeLabel]): Set[InductionHypothesis[VeritasFormula]] =
    (for (el <- els; pinf <- el.propagateInfoList
          if (pinf match {
            case InductionHypothesis(_) => true
            case _ => false
          })) yield
      pinf.asInstanceOf[InductionHypothesis[VeritasFormula]]).toSet

  override def transformProblem(goal: VeritasConstruct, spec: VeritasConstruct, parentedges: Iterable[EdgeLabel], assumptions: Iterable[(EdgeLabel, VeritasConstruct)]): Try[Format] = {
    spec match {
      case m@Module(name, imps, moddefs) => {

        val goaldef: ModuleDef = goal match {
          case g: ModuleDef => g
          case _ => {
            println(s"Goal $goal was not a ModuleDef, defaulting to goal false");
            Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
          }
        }

        //initialize type inference
        val module_tjtranslated = TypingJudgmentTranslator(Seq(m))(defconfig)
        tdcollector(module_tjtranslated)(defconfig).head


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
        // (requires type inference for fixed meta variables - this requires all assumptions

        // get typing rule from goal
        val goaltr = goal match {
          case vf: VeritasFormula => retrieveTypingRule(vf).get
          case _ => sys.error(s"Goal to be transformed (${goal}) did not have the correct format (typing rule required)")
        }

        def extractTypingRulefromIHs(ihs: Set[InductionHypothesis[VeritasFormula]]): Set[TypingRule] =
          for (pih <- ihs) yield {
            val formula = pih.ih
            retrieveTypingRule(formula).get
          }

        //get typing rules from ihs
        val parentihtrs = extractTypingRulefromIHs(parentihs)

        // infer types for the meta variables from typing rule for goal, ihs, and assumptions
        val pre_processed_goaltr = TypingJudgmentTranslator(goaltr)
        val mv_types_goal = tdcollector.inferMetavarTypes(pre_processed_goaltr)
        val mvs_ihs = for (pih <- parentihtrs) yield {
          val pre_processed_pih = TypingJudgmentTranslator(pih)
          tdcollector.inferMetavarTypes(pre_processed_pih)
        }

        //here, we assume that inferred types will not disagree! If they do not, an error should be thrown
        val unionmvtypes: Map[MetaVar, SortRef] = (mv_types_goal +: mvs_ihs.toSeq).fold(Map[MetaVar, SortRef]())((m1, m2) => m1 ++ m2)
        val fixedmvtypes: Map[MetaVar, SortRef] = unionmvtypes.filter(p => parentfvs contains p._1)

        // final consts block with all necessary constant declarations
        val fixedvar_defs: Consts = Consts(
          (for (fv <- parentfvs) yield {
            ConstDecl(fv.name, fixedmvtypes(fv))
          }).toSeq, false)


        //Step 2: make assumptions (Axioms) out of parentihs:

        //for ihs from ihs_parentsonly, fix all variables that appear in parentfvs
        val parentonlyihstrs = extractTypingRulefromIHs(ihs_parentsonly)
        val parentonly_ihs_ax: Iterable[Axioms] = for (ihtr <- parentonlyihstrs) yield Axioms(Seq(fixMVsinVC[TypingRule](ihtr, parentfvs)))

        //for ihs in ihs_intersection, fix all variables in fvs_intersection
        val intersectionihstrs = extractTypingRulefromIHs(ihs_intersection)
        val intersection_ihs_ax: Iterable[Axioms] = for (ihtr <- intersectionihstrs) yield Axioms(Seq(fixMVsinVC[TypingRule](ihtr, fvs_intersection)))


        //Step 3: make assumptions (Axioms) out of child goals:
        //add ihs from ihs_childrenonly as premises to childassms (encode nested implication a => b via !a \/ b)
        //add only IHs from corresponding edges to the assumptions, not the union of all IHs on all edges to children!
        //fix all fixed variables from fvs_intersection, fixed variables from fvs_childrenonly have to be universally quantified
        val assmAxioms: Iterable[Axioms] =
          for ((el, assm) <- assumptions) yield
            assm match {
              case Goals(ax@Seq(tr@TypingRule(name, prems, conseqs)), _) =>
                if (el.propagateInfoList.isEmpty)
                  //no additional premises necessary, only fix fvs in fvs_intersection
                  Axioms(Seq(fixMVsinVC[TypingRule](tr, fvs_intersection)))
                else
                // need to add IHs from ihs_childrenonly as premises to corresponding assumptions
                {
                  val ihs_from_edge: Set[InductionHypothesis[VeritasFormula]] =
                    (el.propagateInfoList filter {
                      case InductionHypothesis(_) => true
                      case _ => false
                    }).toSet map ((pi: PropagatableInfo) => pi.asInstanceOf[InductionHypothesis[VeritasFormula]])

                  val relevant_ihs = ihs_from_edge intersect ihs_childrenonly

                  // create additional premises for the assumptions (encoding nested implication a => b via !a \/ b)
                  // add inner universal quantification for all free variables except the fixed ones (no unsound double quantification of fixed variables!!)
                  val additionalAssms: Seq[TypingRuleJudgment] = (for (ih <- relevant_ihs) yield ih.propagateInfo() match {
                    case Axioms(Seq(TypingRule(_, premises, consequences))) => {
                      val ihprems: Seq[Seq[TypingRuleJudgment]] = premises map (tr => Seq(NotJudgment(tr)))
                      val freevars_in_ih = FreeVariables.freeVariables(ihprems.flatten ++ consequences, childfvs) //ignore variables to be fixed for children (will be quantified universally at top level), quantify all the remaining free variables!
                      ForallJudgment(freevars_in_ih.toSeq, Seq(OrJudgment(ihprems ++ Seq(consequences))))
                    }
                  }).toSeq
                  //finally, fix all the fixed variables that appear in fvs_intersection in the entire assumption
                  Axioms(Seq(fixMVsinVC[TypingRule](TypingRule(name, additionalAssms ++ prems, conseqs), fvs_intersection)))
                }

              case _ => sys.error(s"Assumption $assm in the proof graph had a format which is currently not supported.")
            }

        //Step 4: fix parentfvs in goal
        val final_goal = Goals(Seq(fixMVsinVC[TypingRule](goaltr, parentfvs)), None)

        //Step 5: wrap everything (constant declaration, additional axioms, and transformed goal) in a local block
        // (only if there are fixed variables, otherwise only add the additional axioms for IHs and assupmtions)
        val all_additional_assumptions: Seq[Axioms] =
          (parentonly_ihs_ax ++ intersection_ihs_ax ++ assmAxioms).toSeq

        val final_augmentedgoal: Seq[ModuleDef] =
          if (fixedvar_defs.consts.isEmpty)
            all_additional_assumptions ++ Seq(final_goal)
          else
            Seq(Local(Seq(fixedvar_defs) ++ all_additional_assumptions ++ Seq(goaldef)))


        //wrap everything in a module including the specification
        val module = Module(name + "Transformed", imps, moddefs ++ final_augmentedgoal)

        //finally, transform the entire problem using our standard transformation pipeline
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
