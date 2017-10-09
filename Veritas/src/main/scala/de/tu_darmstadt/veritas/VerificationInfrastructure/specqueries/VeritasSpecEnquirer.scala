package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.{TypingRuleJudgment, _}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}

class VeritasSpecEnquirer(spec: VeritasConstruct) extends SpecEnquirer[VeritasConstruct, VeritasFormula] {

  override val fullspec: VeritasConstruct = spec

  private val defconfig = Configuration(Map(
    Simplification -> Simplification.None,
    VariableEncoding -> VariableEncoding.Unchanged,
    FinalEncoding -> FinalEncoding.TFF,
    Selection -> Selection.SelectAll,
    Problem -> Problem.All))

  private val tdcollector: CollectTypesDefs = new CollectTypesDefsClass

  /**
    * wrap given spec in Module, if it is not already a Module
    * also, apply module to collector of types and definitions in order to make all types and defs accessible via tdcollector
    *
    * @return
    */
  private val richspec: Module = {
    val mod = spec match {
      case m: Module => m
      case mdef: ModuleDef => Module("GenSpecModule", Seq(), Seq(mdef))
      case _ => sys.error("Could not wrap given specification in Module, which is required for VeritasSpecEnquirer.")
    }
    tdcollector(Seq(mod))(defconfig).head
  }

  private def isRecursiveFunction(fname: String): Boolean =
    if (tdcollector.funcdefs.isDefinedAt(fname)) {
      val eqs = tdcollector.funcdefs(fname)
      lazy val allfunccalls = for (eq <- eqs; fcs <- extractFunctionCalls(eq.right)
                                   if fcs.isInstanceOf[FunctionExpApp]) yield fcs.asInstanceOf[FunctionExpApp]
      allfunccalls.exists(fexpapp => fexpapp.functionName == fname)
    }
    else false

  private def retrieveTypingRule(f: VeritasFormula): TypingRule = f match {
    case t@TypingRule(_,_,_) => t
    case tj: TypingRuleJudgment => TypingRule("wrappedTypingRuleJdgm", Seq(), Seq(tj))
    case Goals(Seq(t@TypingRule(_,_,_)),_) => t
    case GoalsWithStrategy(_,Seq(t@TypingRule(_,_,_)),_) => t
    case Lemmas(Seq(t@TypingRule(_,_,_)),_) => t
    case LemmasWithStrategy(_,Seq(t@TypingRule(_,_,_)),_) => t
    case Axioms(Seq(t@TypingRule(_,_,_))) => t
    case _ => sys.error(s"Could not retrieve a typing rule from the given formula: $f")
      //TODO maybe rethink the cases here - do we really need to cover other cases?
  }

  //expects a variable, ask if variable has a type that is a closed ADT
  override def isClosedADT(v: VeritasConstruct, term: VeritasConstruct): Boolean = v match {
    case mv@MetaVar(_) => {
      term match {
        case tr: TypingRule => {
          val varmap = tdcollector.inferMetavarTypes(tr)
          val vartype = varmap(mv)
          !tdcollector.dataTypes(vartype.name)._1
        }
        case _ => sys.error("VeritasSpecEnquirer currently not able to infer variable types from terms that are not TypingRules")
      }
    }
    case _ => false
  }

  override def isForall(g: VeritasFormula): Boolean = g match {
    case TypingRule(_,_,_) => true
    case ForallJudgment(_,_) => true
    case Goals(Seq(TypingRule(_,_,_)), _) => true
    case Axioms(Seq(TypingRule(_,_,_))) => true

    case _ => false
  }

  override def isExists(g: VeritasFormula): Boolean = g match {
    case _: ExistsJudgment => true
    case _ => false
  }

  override def isImplication(g: VeritasFormula): Boolean = g match {
    case _: TypingRule => true
    //TODO maybe also consider implications encoded via disjunction/conjunction?
    case _ => false
  }

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    *
    * @param quantifiedFormula
    * @return
    */
  override def getQuantifiedBody(quantifiedFormula: VeritasFormula): VeritasFormula = quantifiedFormula match {
    case t: TypingRule => t
    case ForallJudgment(_, body) => TypingRule("forallJdg_anonym", Seq(), body) //TODO generate a better, unique name?
    case ExistsJudgment(_, body) => TypingRule("existJdg_anonym", Seq(), body) //TODO generate a better, unique name?
    case _ => sys.error("VeritasSpecEnquirer cannot determine the quantified body of a non-quantified formula")
      // alternatively, maybe simply return the formula that was given without throwing an error?
  }

  override def getArguments(functioncall: VeritasConstruct): Seq[VeritasConstruct] = functioncall match {
    case FunctionExpApp(_, args) => args
    case _ => Seq() //alternatively, throw error or warning here?
  }

  override def getCases(v: VeritasConstruct, term: VeritasConstruct): Seq[VeritasConstruct] = v match {
    case mv@MetaVar(_) => {
      term match {
        case tr: TypingRule => {
          val varmap = tdcollector.inferMetavarTypes(tr)
          val vartype = varmap(mv)
          tdcollector.dataTypes(vartype.name)._2
        }
        case _ => sys.error("VeritasSpecEnquirer currently not able to infer variable types from terms that are not TypingRules")
      }
    }
    case _ => Seq() //alternatively, throw error or warning here?
  }

  override def getRecArgsADT(c: VeritasConstruct): Seq[VeritasConstruct] = ???

  override def getUniversallyQuantifiedVars(g: VeritasFormula): Seq[MetaVar] = g match {
    case t: TypingRule => ???
    case ForallJudgment(vars, _) => vars
    case ExistsJudgment(vars, _) => vars
    case _ => Seq() //alternatively, throw error or warning here?
  }

  override def getPremises(g: VeritasFormula): Seq[VeritasFormula] = g match {
    case TypingRule(_, prems, _) => prems
    case _ => Seq() //alternatively, throw error or warning here?
  }

  override def getConclusions(g: VeritasFormula): Seq[VeritasFormula] = g match {
    case TypingRule(_, _, concs) => concs
    case _ => Seq() //alternatively, throw error or warning here?
  }

  override def getFormulaName(f: VeritasFormula): String = f match {
    case TypingRule(name, _, _) => name
    case _ => sys.error("Cannot determine formula name of Veritas construct that is not a TypingRule")
  }

  override def extractFunctionCalls(s: VeritasConstruct): Seq[VeritasConstruct] = ???

  override def extractFreeVariables(d: VeritasConstruct) = ???

  override def assignCaseVariables[D <: VeritasConstruct](nd: D, refd: D) = ???

  override def makeForall(vars: Seq[VeritasConstruct], body: VeritasFormula): VeritasFormula = body match {
    case TypingRule(_,_,_) => body
    case tjdg: TypingRuleJudgment => TypingRule("forall_anonymous", Seq(), Seq(tjdg)) //TODO better name for generated formula?
    case _ => sys.error("Could not construct a TypingRule with given body")
  }

  override def makeForallQuantifyFreeVariables(body: VeritasFormula, fixed: Seq[VeritasConstruct]) = ???

  override def makeImplication(prems: Seq[VeritasFormula], concs: Seq[VeritasFormula]) = ???

  override def makeEquation(left: VeritasConstruct, right: VeritasConstruct) = ???

  override def makeNamedFormula(f: VeritasFormula, name: String) = ???
}
