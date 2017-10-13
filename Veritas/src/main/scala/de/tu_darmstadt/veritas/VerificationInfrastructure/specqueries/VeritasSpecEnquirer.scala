package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExpApp, FunctionExpEq, FunctionExpMeta}
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

  // list all unquantified variables in the given formula (empty if formula does not have free variables)
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
      case Some(TypingRule(_, Seq(), Seq(ForallJudgment(_, _)))) => true
      case Some(TypingRule(_, Seq(), Seq(ExistsJudgment(_, _)))) => false
      case Some(t@TypingRule(_, _, _)) => getFreeVariables(t).nonEmpty
      case None => false
      //this might not yet cover all the cases as intended
    }

  override def isExists(g: VeritasFormula): Boolean = g match {
    case ExistsJudgment(_, _) => true
    case TypingRule(_, Seq(), Seq(ExistsJudgment(_, _))) => true
    case _ => false
    //this might not yet cover all the cases as intended
  }

  override def isImplication(g: VeritasFormula): Boolean = g match {
    case TypingRule(_, prems, conseqs) => prems.nonEmpty && conseqs.nonEmpty
    //TODO maybe also consider implications encoded via disjunction/conjunction?
    case _ => false
  }

  //determine if a function in the given spec is a recursive function (directly calling itself)
  override def isRecursiveFunctionCall(fc: VeritasConstruct): Boolean =
    fc match {
      case FunctionExpApp(fname, _) => {
        if (tdcollector.funcdefs.isDefinedAt(fname)) {
          val eqs = tdcollector.funcdefs(fname)
          lazy val allfunccalls = for (eq <- eqs;
                                       fcs <- extractFunctionCalls(eq.right)
                                       if fcs.isInstanceOf[FunctionExpApp]) yield fcs.asInstanceOf[FunctionExpApp]
          allfunccalls.exists(fexpapp => fexpapp.functionName == fname)
        }
        else false
      }
      case _ => sys.error("Cannot determine if a construct that is not a FunctionExpApp is a call to a recursive function.")
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
      case TypingRule(name, Seq(), Seq(ForallJudgment(_, body))) => TypingRule(name + "-body", Seq(), body)
      case TypingRule(name, Seq(), Seq(ExistsJudgment(_, body))) => TypingRule(name + "-body", Seq(), body)
      case t@TypingRule(name, _, _) if getFreeVariables(t).nonEmpty => t
      case ForallJudgment(_, body) => TypingRule("forallJdg_anonym-body", Seq(), body) //TODO generate a better, unique name?
      case ExistsJudgment(_, body) => TypingRule("existJdg_anonym-body", Seq(), body) //TODO generate a better, unique name?
      case _ => sys.error("VeritasSpecEnquirer cannot determine the quantified body of a non-quantified formula")
      // alternatively, maybe simply return the formula that was given without throwing an error?
    }

  /**
    * expects a term that is a function application, extracts the arguments from it
    *
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

  //from a named ADT case, extract the recursive arguments (may be empty if there are none)
  //assume unique constructors!
  override def getRecArgsADT(c: VeritasConstruct): Seq[VeritasConstruct] = c match {
    case fexpapp@FunctionExpApp(name, args) => {
      //retrieve datatype that has this constructor (make sure it is only one!)
      val dtmap = for ((dtname, (_, dtconstrs)) <- tdcollector.dataTypes; dtcons <- dtconstrs
                       if dtcons.name == name) yield dtname -> dtcons
      if (dtmap.isEmpty)
        sys.error(s"Could not find a datatype that has constructor $name")
      else if (dtmap.size > 1)
        sys.error(s"Constructor $name is not unique; found in datatypes $dtmap")
      else {
        val (dtname, dtcons) = dtmap.head
        //throw an error if argument sizes don't match up
        if (args.length != dtcons.in.length)
          sys.error(s"Wrong number of arguments for a constructor: $dtcons expected ${dtcons.in.length} arguments, named expression $fexpapp had ${args.length} arguments.")
        //retrieve recursive arguments from arguments
        val recargs = for ((sr, fexpm) <- dtcons.in zip args if sr.name == dtname) yield fexpm
        recargs
      }
    }
    case _ => sys.error("Cannot determine recursive arguments for a construct that is not a FunctionExpApp")
  }

  //expects a universally quantified formula, hands back a list of variables
  // (which we define as not being formulas by themselves - is that a good idea?)
  //for other formulae, returns the empty sequence
  override def getUniversallyQuantifiedVars(g: VeritasFormula): Set[VeritasConstruct] =
  retrieveTypingRule(g) match {
    case Some(tr) => getFreeVariables(tr) map (mv => mv.asInstanceOf[VeritasConstruct]) //TODO: maybe find a solution to get around the manual upcast?
    case None => g match {
      case ForallJudgment(vars, _) => vars.toSet
      case _ => Set() //alternatively, throw error or warning here?
    }
  }

  //expects an implication and returns the sequence of conjuncts from the premise
  // the conjuncts themselves are formulae
  //for other formulae, returns the empty sequence (interpreted as implication with empty premises!)
  override def getPremises(g: VeritasFormula): Seq[VeritasFormula] =
  retrieveTypingRule(g) match {
    case Some(TypingRule(_, prems, _)) => prems
    case None => Seq() //alternatively, throw error or warning here?
  }

  //expects an implication and returns the sequence of conjuncts from the conclusion
  // the conjuncts themselves are formulae
  //for other formulae, returns the given formula
  override def getConclusions(g: VeritasFormula): Seq[VeritasFormula] =
  retrieveTypingRule(g) match {
    case Some(TypingRule(_, _, conseqs)) => conseqs
    case None => Seq(g)
  }

  //expects a construct with a named formula and extracts the formula's name
  override def getFormulaName(f: VeritasFormula): String =
    retrieveTypingRule(f) match {
      case Some(TypingRule(name, _, _)) => name
      case None => sys.error("Cannot get name of an unnamed formula.")
    }

  override def extractFunctionCalls(s: VeritasConstruct): Seq[VeritasConstruct] = ???

  override def assignCaseVariables[D <: VeritasConstruct](nd: D, refd: D) = ???

  override def makeForall(vars: Seq[VeritasConstruct], body: VeritasFormula): VeritasFormula = body match {
    case TypingRule(_, _, _) => body
    case tjdg: TypingRuleJudgment => TypingRule("forall_anonymous", Seq(), Seq(tjdg)) //TODO better name for generated formula?
    case _ => sys.error("Could not construct a TypingRule with given body")
  }

  //constructs a universally quantified formula where all free variables will be quantified
  //except for the ones which are fixed variables (have to become constants, for example!)
  override def makeForallQuantifyFreeVariables(body: VeritasFormula, fixed: Seq[VeritasConstruct]) = ???

  //for Veritas ASTs: try to cast all prems and concs to TypingRuleJudgments, then create a TypingRule with a generic name
  override def makeImplication(prems: Seq[VeritasFormula], concs: Seq[VeritasFormula]): VeritasFormula = {
    //TODO: maybe make this function a bit more generous later on, so that it actually attempts to extract sensible Seq[TypingRuleJudgment] from the given arguments
    try {
      val tjprems = for (p <- prems) yield p.asInstanceOf[TypingRuleJudgment]
      val tjconcs = for (c <- concs) yield c.asInstanceOf[TypingRuleJudgment]
      TypingRule("generatedImplication_anon", tjprems, tjconcs)
    } catch {
      case ex: ClassCastException => sys.error("When trying to create an implication, was unable to cast all of the given premises/conclusions to TypingRuleJudgments")
    }
  }

  override def makeEquation(left: VeritasConstruct, right: VeritasConstruct): VeritasFormula =
    (left, right) match {
      case (l: FunctionExpMeta, r: FunctionExpMeta) => FunctionExpJudgment(FunctionExpEq(l, r))
      case _ => sys.error(s"Unable to cast $left or $right into a FunctionExpMeta for constructing an equation.")
    }


  override def makeNamedFormula(f: VeritasFormula, name: String): VeritasFormula = ???
}
