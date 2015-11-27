package de.tu_darmstadt.veritas.backend.transformation.collect

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError

trait CollectTypes extends ModuleTransformation {
  private var _dataTypes: Set[String] = Set()
  // constrTypes: datatype constructors and constants
  private var _constrTypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  private var _functypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  private var _pfunctypes: Map[String, (Seq[SortRef], SortRef)] = Map()

  def dataTypes = _dataTypes
  def constrTypes = _constrTypes
  def functypes = _functypes
  def pfunctypes = _pfunctypes

  /**
   * top-level function for translating a Module to a TffFile
   */
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = {
    _dataTypes = Set()
    _constrTypes = Map()
    _functypes = Map()
    _pfunctypes = Map()
    super.transModule(name, is, mdefs)
  }

  def symbolType(name: String) = constrTypes.get(name) match {
    case Some(t) => Some(t)
    case None => functypes.get(name) match {
      case Some(t) => Some(t)
      case None => pfunctypes.get(name)
    }
  }
  
  /**
   * Make sure that type symbols are properly scoped by local and strategy blocks
   */
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case d: DataType => 
      _dataTypes += d.name
      super.transModuleDefs(d)
    case Local(_) | Strategy(_,_,_) => 
      val oldDataTypes = _dataTypes
      val oldconstypes = _constrTypes
      val oldfunctypes = _functypes
      val oldpfunctypes = _pfunctypes
      val res = super.transModuleDefs(mdef)
      _dataTypes = oldDataTypes
      _constrTypes = oldconstypes
      _functypes = oldfunctypes
      _pfunctypes = oldpfunctypes
      res
    case _ => 
      super.transModuleDefs(mdef)
  }

  override def transDataTypeConstructor(d: DataTypeConstructor, open: Boolean, dataType: String): Seq[DataTypeConstructor] = {
    withSuper(super.transDataTypeConstructor(d, open, dataType)) {
      case d =>
        _constrTypes += (d.name -> (d.in -> SortRef(dataType)))
        Seq(d)
    }
  }

  override def transConstDecl(d: ConstDecl): Seq[ConstDecl] = {
    withSuper(super.transConstDecl(d)) {
      case d =>
        _constrTypes += (d.name -> (Seq() -> d.out))
        Seq(d)
    }
  }

  override def transSortDefs(sd: SortDef): Seq[SortDef] =
    withSuper(super.transSortDefs(sd)) {
      case sd =>
        _dataTypes += sd.name
        Seq(sd)
    }

  override def transFunctionSig(sig: FunctionSig): FunctionSig =
    withSuper(super.transFunctionSig(sig)) {
      case sig =>
        _functypes += (sig.name -> (sig.in, sig.out))
        sig
    }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] =
    withSuper(super.transTypingRules(tr)) {
      case tr =>
        Seq(tr)
    }

  def inferMetavarTypes(tr: TypingRule): Map[MetaVar, SortRef] = {
    val jdgs = tr.premises ++ tr.consequences
    val vars = FreeVariables.freeVariables(jdgs)
    inferMetavarTypes(vars, jdgs)
  }

  def inferMetavarTypes(vars: Iterable[MetaVar], jdgs: Seq[TypingRuleJudgment]): Map[MetaVar, SortRef] = {
    Map() ++ (for (v <- vars) yield {
      val occurrences = findTypableOccurrences(v, jdgs)
      val typelist = occurrences flatMap (typeOcc(v, _))
      val typeset = typelist.toSet //remove duplicate occurrences of found types; ideally, only one occurrence should be left!
      typeset.size match {
        case 0 => throw TransformationError(s"Could not infer type of metavariable ${v.name}, no candidate found.") //has to be an error case, untyped variables will not be supported by e.g. Vampire!
        case 1 => (v -> typeset.head)
        case _ => throw TransformationError(s"Could not infer type of metavariable ${v.name}, multiple candidates found $typeset")
      }
    })
  }
  
  /**
   * finds constructs in given sequence of TypingRuleJudgment that have the
   * given MetaVar m as direct child and are one of
   * - function applications (Appl)
   * - equalities/inequalities with the m on one side and a function application on the other side
   *
   */
  private def findTypableOccurrences(m: MetaVar, jdglist: Seq[TypingRuleJudgment]): Set[FunctionExp] = {
    def containsMetaVar(args: Seq[FunctionExpMeta]): Boolean = args exists
      {
        case FunctionMeta(m0) => m == m0
        case _ => false
      }

    def searchFunctionExp(e: FunctionExpMeta): Set[FunctionExp] = {
      e match {
        // base cases just return empty set, all listed explicitly just in case!  
        case FunctionMeta(m) => Set()
        case FunctionExpVar(n) => Set()
        case FunctionExpTrue => Set()
        case FunctionExpFalse => Set()
        case FunctionExpNot(f) => searchFunctionExp(f)
        case fe @ FunctionExpEq(FunctionMeta(mx), r @ FunctionExpApp(n, args)) =>
          if (mx == m) Set(fe) else searchFunctionExp(r)
        case fe @ FunctionExpEq(l @ FunctionExpApp(n, args), r @ FunctionMeta(mx)) =>
          if (mx == m) Set(FunctionExpEq(r, l)) else searchFunctionExp(l)
        case FunctionExpEq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case fe @ FunctionExpNeq(FunctionMeta(mx), r @ FunctionExpApp(n, args)) =>
          if (mx == m) Set(fe) else searchFunctionExp(r)
        case fe @ FunctionExpNeq(l @ FunctionExpApp(n, args), r @ FunctionMeta(mx)) =>
          if (mx == m) Set(FunctionExpNeq(r, l)) else searchFunctionExp(l)
        case FunctionExpNeq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case FunctionExpAnd(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpOr(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpBiImpl(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpIf(c, t, e) => searchFunctionExp(c) ++ searchFunctionExp(t) ++ searchFunctionExp(e)
        case FunctionExpLet(n, e, i) => searchFunctionExp(e) ++ searchFunctionExp(i)
        case fe @ FunctionExpApp(fn, args) => {
          val afe = (args flatMap searchFunctionExp).toSet
          if (containsMetaVar(args)) afe ++ Set(fe) else afe
        }
        // this should never happen
        case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, found a function expression $e that is not covered by the code!")
      }
    }
    (for (jdg <- jdglist) yield {
      jdg match {
        case FunctionExpJudgment(f) => searchFunctionExp(f)
        case ExistsJudgment(vl, jdgl) => findTypableOccurrences(m, jdgl)
        case ForallJudgment(vl, jdgl) => findTypableOccurrences(m, jdgl)
        case NotJudgment(jdg) => findTypableOccurrences(m, Seq(jdg))
        case OrJudgment(jdgll) => jdgll flatMap (findTypableOccurrences(m, _))
        case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, encountered a construct in an axiom, premise, or conclusion that is not supported by core modules!")
      }
    }).flatten.toSet
  }

  /**
   * given a FunctionExp such as the ones found by findTypableOccurrences,
   * tries to determine the type of the given MetaVar m
   */
  private def typeOcc(m: MetaVar, occ: FunctionExp): Option[SortRef] = {

    def getMetaVarPos(args: Seq[FunctionExpMeta]): Seq[Int] =
      for (i <- args.indices if (args(i) == FunctionMeta(m))) yield i

    def getReturnType(f: String): Option[SortRef] = {
      symbolType(f) map (_._2)
    }

    def getArgType(fn: String, args: Seq[FunctionExpMeta]): Option[SortRef] = {
      symbolType(fn) match {
        case None => None
        case Some(t) =>
          val poslist = getMetaVarPos(args)
          val typeset = (poslist map (p => t._1(p))).toSet
          if (typeset.size == 1)
            Some(typeset.head) 
          else 
            None            
      }
    }

    occ match {
      case FunctionExpEq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(n)
      case FunctionExpNeq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(n)
      case FunctionExpApp(fn, args) => getArgType(fn, args)
      case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, an untypable FunctionExp was marked as typable.")
    }
  }
}

class CollectTypesClass extends CollectTypes