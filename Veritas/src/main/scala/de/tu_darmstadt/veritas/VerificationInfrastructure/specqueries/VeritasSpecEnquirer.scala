package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.FreshVariables
import de.tu_darmstadt.veritas.backend.{Configuration, util}
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, _}
import de.tu_darmstadt.veritas.backend.ast.{TypingRuleJudgment, _}
import de.tu_darmstadt.veritas.backend.transformation.{ModuleTransformation, SeqTrans}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}
import de.tu_darmstadt.veritas.backend.transformation.defs.{InferTypingJudgmentsSignature, TranslateAllTypingJudgments, TranslateTypingJudgments}
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.VarToApp0
import de.tu_darmstadt.veritas.backend.util.{FreeVariables, FreshNames}

class VeritasSpecEnquirer(spec: VeritasConstruct) extends SpecEnquirer[VeritasConstruct, VeritasFormula] {

  override val fullspec: VeritasConstruct = spec

  private val defconfig = Configuration(Map(
    Simplification -> Simplification.None,
    VariableEncoding -> VariableEncoding.Unchanged,
    FinalEncoding -> FinalEncoding.TFF,
    Selection -> Selection.SelectAll,
    Problem -> Problem.All))

  //for inferring types of functions and datatypes
  protected val tdcollector: CollectTypesDefs = new CollectTypesDefsClass with Serializable

  //when inferring meta variables of a typing rule, prepare the rule so that meta vars can be inferred
  //requires removing any FunctionExpVar constructs and translating typing judgments to functions/predicates
  private object PrepareMetaVarInference extends SeqTrans(VarToApp0, TranslateAllTypingJudgments) {
    //convenience method for being able to apply the translator directly to a typing rule
    def apply(tr: TypingRule): TypingRule = {
      ts.foldLeft(tr)((t1, t2) => t2.transTypingRules(t1).head)
    }
  }

  /**
    * wrap given spec in Module, if it is not already a Module
    * also, apply module to collector of types and definitions in order to make all types and defs accessible via tdcollector
    * (including inference of signature of typing judgment and placing the function signatures in tdcollector)
    *
    * @return
    */
  private val richspec: Module = {
    val mod = spec match {
      case m: Module => m
      case mdef: ModuleDef => Module("GenSpecModule", Seq(), Seq(mdef))
      case _ => sys.error("Could not wrap given specification in Module, which is required for VeritasSpecEnquirer.")
    }

    val module_tjtranslated = PrepareMetaVarInference(Seq(mod))(defconfig) //once at instantiation time: infer signature of typing judgment over entire spec
    tdcollector(module_tjtranslated)(defconfig).head //once at instantiation time: collect all datatype/function definitions etc. over entire spec (including signatures of typing judgments, inferred previously)
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
  private def getFreeVariables(f: VeritasFormula, ignore: Set[MetaVar] = Set()): Set[MetaVar] =
    retrieveTypingRule(f) match {
      case Some(TypingRule(_, prems, conseqs)) => FreeVariables.freeVariables(prems ++ conseqs, ignore)
      case None => f match {
        case tr: TypingRuleJudgment => FreeVariables.freeVariables(Seq(tr), ignore)
        case _ => Set() //should not happen
      }
    }

  // get types of all variables (free and quantified)
  def getAllVarTypes(f: VeritasFormula): Map[MetaVar, SortRef] =
    retrieveTypingRule(f) match {
      case Some(tr@TypingRule(_, _, _)) => {
        //first, preprocess typing rule (translate typing judgments)
        val processedrule = PrepareMetaVarInference(tr)
        tdcollector.inferMetavarTypes(processedrule)
      }
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


  override def isNegation(g: VeritasFormula): Boolean = g match {
    case FunctionExpJudgment(FunctionExpNot(_)) => true
    case FunctionExpJudgment(FunctionExpNeq(_, _)) => true
    case FunctionExpJudgment(_) => false
    case _ => false
  }

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    *
    * @param quantifiedFormula
    * @return
    */
  override def getQuantifiedBody(quantifiedFormula: VeritasFormula): VeritasFormula =
    retrieveTypingRule(quantifiedFormula) match {
      //first two cases are improvised, since Veritas ASTs currently have no separate conjunction construct,
      //hence we cannot simply return the body of a quantified judgment (would be Seq[VeritasFormula])
      //careful, this essentially throws quantification away!
      case Some(TypingRule(name, Seq(), Seq(ForallJudgment(_, body)))) => TypingRule(name + "-body", Seq(), body)
      case Some(TypingRule(name, Seq(), Seq(ExistsJudgment(_, body)))) => TypingRule(name + "-body", Seq(), body)
      case Some(t@TypingRule(name, _, _)) if getFreeVariables(t).nonEmpty => t
      case None => sys.error(s"VeritasSpecEnquirer cannot determine the quantified body of a non-quantified formula: $quantifiedFormula")
      // alternatively, maybe simply return the formula that was given without throwing an error?
    }

  /**
    * expects a term that is a function application, extracts the arguments from it
    *
    * @param functioncall
    * @return
    */
  override def getArguments(functioncall: VeritasConstruct): Seq[VeritasConstruct] = functioncall match {
    case FunctionExpApp(_, args) => args map {
      case FunctionMeta(mv) => mv //make sure to unwrap MetaVars!
      case cs => cs
    }
    case _ => Seq() //alternatively, throw error or warning here?
  }

  // for a variable of type closed ADT, extract the different cases (variable v is typed as in the given term)
  override def getCases(v: VeritasConstruct, term: VeritasConstruct): Map[String, VeritasConstruct] = v match {
    case mv@MetaVar(_) => {
      term match {
        case f: VeritasFormula => {
          val varmap = getAllVarTypes(f)
          val vartype = varmap(mv) //this could fail if the given variable does not appear in the given term!
          val constructors = tdcollector.dataTypes(vartype.name)._2
          (for (c <- constructors) yield (c.name, c)).toMap
        }
        case _ => sys.error("VeritasSpecEnquirer currently not able to infer variable types from VeritasFormula")
      }
    }
    case _ => Map() //alternatively, throw error or warning here?
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


  override def getVarName(f: VeritasConstruct): String = f match {
    case MetaVar(n) => n
    case FunctionMeta(MetaVar(n)) => n
    case _ => sys.error(s"Could not extract variable name from given VeritasConstruct $f")
  }

  //from a given definition, extract all the calls to functions
  override def extractFunctionCalls(s: VeritasConstruct): Seq[VeritasConstruct] = {
    val functionCallExtractor = new VeritasConstructTraverser {
      override def transFunctionExp(f: FunctionExp): FunctionExp =
        withSuper(super.transFunctionExp(f)) {
          case fea@FunctionExpApp(fn, args) => {
            //only collect calls to functions, not datatype constructors!
            if (tdcollector.functypes.contains(fn) || tdcollector.pfunctypes.contains(fn))
              collected = collected :+ fea
            FunctionExpApp(fn, trace(args)(transFunctionExpMetas))
          }
        }

      override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
        withSuper(super.transFunctionExps(f)) {
          case fea@FunctionExpApp(fn, args) => {
            //only collect calls to functions, not datatype constructors!
            if (tdcollector.functypes.contains(fn) || tdcollector.pfunctypes.contains(fn))
              collected = collected :+ fea
            Seq(FunctionExpApp(fn, trace(args)(transFunctionExpMetas)))
          }
        }
    }
    functionCallExtractor(s)
    functionCallExtractor.collected
  }

  //from a given definition, extract all usages of constructors
  override def extractConstructorUsages(s: VeritasConstruct): Seq[VeritasConstruct] = {
    val constructorUsagesExtractor = new VeritasConstructTraverser {
      override def transFunctionExp(f: FunctionExp): FunctionExp =
        withSuper(super.transFunctionExp(f)) {
          case fea@FunctionExpApp(fn, args) => {
            //only collect calls to functions, not datatype constructors!
            if (tdcollector.constrTypes.contains(fn))
              collected = collected :+ fea
            FunctionExpApp(fn, trace(args)(transFunctionExpMetas))
          }
        }

      override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
        withSuper(super.transFunctionExps(f)) {
          case fea@FunctionExpApp(fn, args) => {
            //only collect calls to functions, not datatype constructors!
            if (tdcollector.constrTypes.contains(fn))
              collected = collected :+ fea
            Seq(FunctionExpApp(fn, trace(args)(transFunctionExpMetas)))
          }
        }
    }
    constructorUsagesExtractor(s)
    constructorUsagesExtractor.collected
  }

  override def assignCaseVariables(nd: VeritasConstruct, refd: VeritasConstruct): VeritasConstruct =
    nd match {
      case DataTypeConstructor(name, args) => {
        //obtain names of ALL variables in refd
        // first the free vars:
        val freevars = refd match {
          case TypingRule(_, prems, conseqs) => FreeVariables.freeVariables(prems ++ conseqs, Set(): Set[MetaVar])
          case trj: TypingRuleJudgment => FreeVariables.freeVariables(trj, Set(): Set[MetaVar])
          case fem: FunctionExpMeta => FreeVariables.freeVariables(fem, Set(): Set[MetaVar])
          case _ => sys.error(s"Cannot retrieve free variables from this expression $refd")
        }

        // finally, also quantified names:
        val qvarsextractor = new VeritasConstructTraverser {
          override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
            withSuper(super.transTypingRuleJudgment(trj)) {
              case ExistsJudgment(vl, jl) => {
                collected = collected ++ vl
                ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
              }
              case ForallJudgment(vl, jl) => {
                collected = collected ++ vl
                ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
              }
            }

          override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
            withSuper(super.transTypingRuleJudgments(trj)) {
              case ExistsJudgment(vl, jl) => {
                collected = collected ++ vl
                Seq(ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_))))
              }
              case ForallJudgment(vl, jl) => {
                collected = collected ++ vl
                Seq(ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_))))
              }
            }
        }

        qvarsextractor(refd)

        val quantifiednames: Set[MetaVar] = (qvarsextractor.collected map (_.asInstanceOf[MetaVar])).toSet

        val freshargnames = FreshVariables.freshMetaVars(freevars ++ quantifiednames, tdcollector.constrTypes(name)._1)

        FunctionExpApp(name, freshargnames map (n => FunctionMeta(n)))
      }
      case _ => sys.error("Can only assign variables to DataTypeConstructor")
    }

  override def makeForall(vars: Seq[VeritasConstruct], body: VeritasFormula): VeritasFormula = body match {
    case TypingRule(_, _, _) => body
    case tjdg: TypingRuleJudgment => TypingRule("forall_anonymous", Seq(), Seq(tjdg)) //TODO better name for generated formula?
    case _ => sys.error("Could not construct a TypingRule with given body")
  }

  //constructs a universally quantified formula where all free variables will be quantified
  //except for the ones which are fixed variables (have to become constants, for example!)
  //Veritas format can ignore parameter fixed, will be correctly handled during transformation
  override def makeForallQuantifyFreeVariables(body: VeritasFormula, fixed: Seq[VeritasConstruct]): VeritasFormula =
    retrieveTypingRule(body).getOrElse(sys.error(s"Could not make the given formula $body into a typing rule."))

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
      case (l: MetaVar, r: FunctionExpMeta) => FunctionExpJudgment(FunctionExpEq(FunctionMeta(l), convertAllFunExpVarsToMetaVars(r)))
      case (l: FunctionExpMeta, r: MetaVar) => FunctionExpJudgment(FunctionExpEq(convertAllFunExpVarsToMetaVars(l), FunctionMeta(r)))
      case (l: FunctionExpMeta, r: FunctionExpMeta) => FunctionExpJudgment(FunctionExpEq(convertAllFunExpVarsToMetaVars(l), convertAllFunExpVarsToMetaVars(r)))
      case _ => sys.error(s"Unable to cast $left or $right into a FunctionExpMeta for constructing an equation.")
    }

  override def makeInequation(left: VeritasConstruct, right: VeritasConstruct): VeritasFormula =
    (left, right) match {
      case (l: MetaVar, r: FunctionExpMeta) => FunctionExpJudgment(FunctionExpNeq(FunctionMeta(l), convertAllFunExpVarsToMetaVars(r)))
      case (l: FunctionExpMeta, r: MetaVar) => FunctionExpJudgment(FunctionExpNeq(convertAllFunExpVarsToMetaVars(l), FunctionMeta(r)))
      case (l: FunctionExpMeta, r: FunctionExpMeta) => FunctionExpJudgment(FunctionExpNeq(convertAllFunExpVarsToMetaVars(l), convertAllFunExpVarsToMetaVars(r)))
      case _ => sys.error(s"Unable to cast $left or $right into a FunctionExpMeta for constructing an inequation.")
    }

  //expects an unnamed Formulae and generates an appropriate name
  //for Veritas language: expects judgments (which are by definition unnamed)
  //currently limited to FunctionExpJudgments!
  //generate name according to constructors called; if no constructors called, generate name according to functions called
  //if no functions called, attach (unique) metavariable names used
  override def makeFormulaName(f: VeritasFormula): String = f match {
    case FunctionExpJudgment(fexp) => {
      val constructorUsages = extractConstructorUsages(fexp)
      if (constructorUsages.nonEmpty) {
        val names = for (c <- constructorUsages) yield c match {
          case FunctionExpApp(n, _) => n
        }
        names.mkString("-")
      } else {
        val functionCalls = extractFunctionCalls(fexp)
        if (functionCalls.nonEmpty) {
          val names = for (c <- functionCalls) yield c match {
            case FunctionExpApp(n, _) => n
          }
          names.mkString("-")
        } else {
          val vars = getFreeVariables(f)
          //collect unique variable names
          val varnames = (for (v <- vars) yield getVarName(v)).toSet
          varnames.mkString("-")
        }
      }
    }
    case _ => sys.error(s"Currently unable to generate a name for formula $f")
  }


  //expects an unnamed formula or a named one and attaches or overwrites the new name, always creating a typing rule
  private def makeNamedFormula(f: VeritasFormula, name: String): TypingRule = retrieveTypingRule(f) match {
    case Some(TypingRule(_, prems, conseqs)) => TypingRule(name, prems, conseqs)
    case None => sys.error(s"Was unable to produce a named formula out of $f") //should not happen
  }

  //expects an unnamed formula or a named one and attaches or overwrites the new name, producing a goal
  override def makeNamedGoal(f: VeritasFormula, name: String): VeritasFormula =
    Goals(Seq(makeNamedFormula(f, name)), None)

  //expects an unnamed formula or a named one and attaches or overwrites the new name, producing a goal
  def makeNamedAxiom(f: VeritasFormula, name: String): VeritasFormula =
    Axioms(Seq(makeNamedFormula(f, name)))

  override def makeMVTerm(s: String): VeritasConstruct = MetaVar(s)

  private def convertAllFunExpVarsToMetaVars(fexp: FunctionExp): FunctionExp = {
    val transformer = new VeritasConstructTraverser {
      override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
        withSuper(super.transFunctionExpMeta(f)) {
          case FunctionExpVar(n) => FunctionMeta(MetaVar(n))
        }


      override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
        withSuper(super.transFunctionExpMetas(f)) {
          case FunctionExpVar(n) => Seq(FunctionMeta(MetaVar(n)))
        }


    }

    transformer.transFunctionExp(fexp)
  }

  private def convertAllFunExpVarsToMetaVars(fexpm: FunctionExpMeta): FunctionExpMeta = {
    val transformer = new VeritasConstructTraverser {
      override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
        withSuper(super.transFunctionExpMeta(f)) {
          case FunctionExpVar(n) => FunctionMeta(MetaVar(n))
        }


      override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
        withSuper(super.transFunctionExpMetas(f)) {
          case FunctionExpVar(n) => Seq(FunctionMeta(MetaVar(n)))
        }


    }

    transformer.transFunctionExpMeta(fexpm)
  }

  def convertExpToFormula(body: VeritasConstruct): VeritasFormula =
    body match {
      case exp: FunctionExp => FunctionExpJudgment(convertAllFunExpVarsToMetaVars(exp))
      case _ => sys.error(s"Unable to cast $body into a FunctionExp for constructing a function expression.")
    }

  //currently only covers relevant cases existing in SQL/QL proofs
  def convertExpToNegFormula(body: VeritasConstruct, context: Seq[VeritasFormula]): VeritasFormula = {

    //from formula's context, compute variable names to be ignored during quantification
    val ignore = (for (c <- context) yield getFreeVariables(c)).flatten.toSet

    def quantifyVars(f: VeritasFormula): Set[MetaVar] =
      getFreeVariables(f, ignore)

    def quantifyIfNecessary(f: TypingRuleJudgment, subf: FunctionExpMeta): VeritasFormula = {
      val varsToQuantify = subf match {
        case fexp: FunctionExp => quantifyVars(FunctionExpJudgment(convertAllFunExpVarsToMetaVars(fexp)))
        case _ => Set()
      }

      if (varsToQuantify.nonEmpty)
        ForallJudgment(varsToQuantify.toSeq, Seq(f))
      else
        f
    }

    body match {
      case FunctionExpEq(l, r) => quantifyIfNecessary(FunctionExpJudgment(FunctionExpNeq(convertAllFunExpVarsToMetaVars(l), convertAllFunExpVarsToMetaVars(r))), r)
      case exp: FunctionExp => quantifyIfNecessary(FunctionExpJudgment(FunctionExpNot(convertAllFunExpVarsToMetaVars(exp))), exp)
      case FunctionExpJudgment(FunctionExpEq(l, r)) => quantifyIfNecessary(FunctionExpJudgment(FunctionExpNeq(convertAllFunExpVarsToMetaVars(l), convertAllFunExpVarsToMetaVars(r))), r)
      case FunctionExpJudgment(exp: FunctionExp) => quantifyIfNecessary(FunctionExpJudgment(FunctionExpNot(convertAllFunExpVarsToMetaVars(exp))), exp)
      case _ => sys.error(s"Unable to cast $body into a FunctionExp for constructing a negation.")
    }
  }


}
