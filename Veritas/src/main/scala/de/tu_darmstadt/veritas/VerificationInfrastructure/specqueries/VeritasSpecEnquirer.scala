package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.{TypingRuleJudgment, _}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

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

  //determine if a function in the given spec is a recursive function (directly calling itself)
  private def isRecursiveFunction(fname: String): Boolean =
    if (tdcollector.funcdefs.isDefinedAt(fname)) {
      val eqs = tdcollector.funcdefs(fname)
      lazy val allfunccalls = for (eq <- eqs; fcs <- extractFunctionCalls(eq.right)
                                   if fcs.isInstanceOf[FunctionExpApp]) yield fcs.asInstanceOf[FunctionExpApp]
      allfunccalls.exists(fexpapp => fexpapp.functionName == fname)
    }
    else false

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

  // list all unquantified variables in the given formula (empty if formula does not have free variables
  private def getFreeVariables(f: VeritasFormula): Set[MetaVar] =
    retrieveTypingRule(f) match {
      case Some(TypingRule(_, prems, conseqs)) => FreeVariables.freeVariables(prems ++ conseqs)
      case None => Set() //should not happen
    }

  // get types of all variables (free and quantified)
  private def getAllVarTypes(f: VeritasFormula): Map[MetaVar, SortRef] =
    retrieveTypingRule(f) match {
      case Some(tr@TypingRule(_, _, _)) => tdcollector.inferMetavarTypes(tr)
      case None => Map() //should not happen
    }

  // list all free variables with their types in the given formula (empty map if formula does not have free variables)
  private def getFreeTypedVariables(f: VeritasFormula): Map[MetaVar, SortRef] = {
    val freevars = getFreeVariables(f)
    for ((mv, sv) <- getAllVarTypes(f); if freevars contains mv) yield (mv, sv)
  }

  //expects a variable, ask if variable has a type that is a closed ADT
  override def isClosedADT(v: VeritasConstruct, term: VeritasConstruct): Boolean = v match {
    case mv@MetaVar(_) => {
      term match {
        case f: VeritasFormula => {
          val varmap = getAllVarTypes(f)
          val vartype = varmap(mv) //this could fail if the given variable does not appear in the given term!
          !tdcollector.dataTypes(vartype.name)._1
        }
        case _ => sys.error("VeritasSpecEnquirer currently not able to infer variable types from VeritasFormula")
      }
    }
    case _ => false
  }


  override def isForall(g: VeritasFormula): Boolean =
    retrieveTypingRule(g) match {
      case Some(TypingRule(_, Seq(), ForallJudgment(_, _))) => true
      case Some(TypingRule(_, Seq(), ExistsJudgment(_, _))) => false
      case Some(t@TypingRule(_, _, _)) => getFreeVariables(t).nonEmpty
      case None => false
      //this might not yet cover all the cases as intended
    }

  override def isExists(g: VeritasFormula): Boolean = g match {
    case ExistsJudgment(_, _) => true
    case TypingRule(_, Seq(), ExistsJudgment(_, _)) => true
    case _ => false
    //this might not yet cover all the cases as intended
  }

  override def isImplication(g: VeritasFormula): Boolean = g match {
    case TypingRule(_, prems, conseqs) => prems.nonEmpty && conseqs.nonEmpty
    //TODO maybe also consider implications encoded via disjunction/conjunction?
    case _ => false
  }

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    *
    * @param quantifiedFormula
    * @return
    */
  override def getQuantifiedBody(quantifiedFormula: VeritasFormula): VeritasFormula =
    quantifiedFormula match {
        //first two cases are improvised, since Veritas ASTs currently have no separate conjunction construct,
        //hence we cannot simply return the body of a quantified judgment (would be Seq[VeritasFormula])
        //careful, this essentially throws quantification away!
    case TypingRule(name, Seq(), ForallJudgment(_, body)) => TypingRule(name + "-body", Seq(), body)
    case TypingRule(name, Seq(), ExistsJudgment(_, body)) => TypingRule(name + "-body", Seq(), body)
    case t@TypingRule(name, _, _) if getFreeVariables(t).nonEmpty => t
    case ForallJudgment(_, body) => TypingRule("forallJdg_anonym-body", Seq(), body) //TODO generate a better, unique name?
    case ExistsJudgment(_, body) => TypingRule("existJdg_anonym-body", Seq(), body) //TODO generate a better, unique name?
    case _ => sys.error("VeritasSpecEnquirer cannot determine the quantified body of a non-quantified formula")
    // alternatively, maybe simply return the formula that was given without throwing an error?
  }

  /**
    * expects a term that is a function application, extracts the arguments from it
    * @param functioncall
    * @return
    */
  override def getArguments(functioncall: VeritasConstruct): Seq[VeritasConstruct] = functioncall match {
    case FunctionExpApp(_, args) => args
    case _ => Seq() //alternatively, throw error or warning here?
  }

  // for a variable of type closed ADT, extract the different cases (variable is typed as in the given term)
  override def getCases(v: VeritasConstruct, term: VeritasConstruct): Seq[VeritasConstruct] = v match {
    case mv@MetaVar(_) => {
      term match {
        case f: VeritasFormula => {
          val varmap = getAllVarTypes(f)
          val vartype = varmap(mv) //this could fail if the given variable does not appear in the given term!
          tdcollector.dataTypes(vartype.name)._2
        }
        case _ => sys.error("VeritasSpecEnquirer currently not able to infer variable types from VeritasFormula")
      }
    }
    case _ => Seq() //alternatively, throw error or warning here?
  }

  //from an ADT case, extract the recursive arguments (may be empty if there are none)
  //assume unique constructors!
  override def getRecArgsADT(c: VeritasConstruct): Seq[VeritasConstruct] = c match {
    case DataTypeConstructor(name, args) => ??? //first retrieve datatype that has this constructor!
    case _ => sys.error("Cannot determine recursive arguments for a construct that is not a DataTypeConstructor")
  }

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
    case TypingRule(_, _, _) => body
    case tjdg: TypingRuleJudgment => TypingRule("forall_anonymous", Seq(), Seq(tjdg)) //TODO better name for generated formula?
    case _ => sys.error("Could not construct a TypingRule with given body")
  }

  override def makeForallQuantifyFreeVariables(body: VeritasFormula, fixed: Seq[VeritasConstruct]) = ???

  override def makeImplication(prems: Seq[VeritasFormula], concs: Seq[VeritasFormula]) = ???

  override def makeEquation(left: VeritasConstruct, right: VeritasConstruct) = ???

  override def makeNamedFormula(f: VeritasFormula, name: String) = ???
}
