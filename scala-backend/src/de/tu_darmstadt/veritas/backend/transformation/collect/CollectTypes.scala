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

  def symbolType(name: String) = constrTypes.get(name) match {
    case Some(t) => Some(t)
    case None => functypes.get(name) match {
      case Some(t) => Some(t)
      case None => pfunctypes.get(name)
    }
  }
  

  /**
   * top-level function for translating a Module to a TffFile
   */
  override def transModule(name: String, _is: Seq[Import], _mdefs: Seq[ModuleDef]): Seq[Module] = {
    _dataTypes = Set()
    _constrTypes = Map()
    _functypes = Map()
    _pfunctypes = Map()
    
    val is = trace(_is)(transModuleImport(_))
    val mdefs = transModuleTypedDefs(_mdefs)
    Seq(Module(name, is, mdefs))
  }

  /**
   * Make sure that type symbols are properly scoped by local and strategy blocks
   */
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case d: DataType => 
      _dataTypes += d.name
      super.transModuleDefs(d)
      
    case Local(defs) =>
      val oldDataTypes = _dataTypes
      val oldconstypes = _constrTypes
      val oldfunctypes = _functypes
      val oldpfunctypes = _pfunctypes
      val res = transModuleTypedDefs(defs)
      _dataTypes = oldDataTypes
      _constrTypes = oldconstypes
      _functypes = oldfunctypes
      _pfunctypes = oldpfunctypes
      Seq(Local(res))

    case Strategy(name,is,defs) => 
      val oldDataTypes = _dataTypes
      val oldconstypes = _constrTypes
      val oldfunctypes = _functypes
      val oldpfunctypes = _pfunctypes
      val res = transModuleTypedDefs(defs)
      _dataTypes = oldDataTypes
      _constrTypes = oldconstypes
      _functypes = oldfunctypes
      _pfunctypes = oldpfunctypes
      Seq(Strategy(name, is, res))
      
    case _ => 
      super.transModuleDefs(mdef)
  }

  /**
   * Visit type declarations first.
   */
  def transModuleTypedDefs(mdefs: Seq[ModuleDef]): Seq[ModuleDef] = {
    val mdefsTyped = mdefs map { mdef => mdef match {
      case DataType(_,_,_)
         | Functions(_)
         | PartialFunctions(_)
         | Sorts(_)
        => Left(transModuleDefs(mdef)) // Left = done in first phase
      case other => Right(Seq(other))  // Right = to be done in second phase
    }}
    mdefsTyped flatMap {
      case Left(seq) => seq
      case Right(Seq(mdef)) => transModuleDefs(mdef)
    }
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

  def inferMetavarTypes(tr: TypingRule) = 
    new TypeInference(symbolType).inferMetavarTypes(tr)

  def inferMetavarTypes(vars: Iterable[MetaVar], jdgs: Seq[TypingRuleJudgment]) = 
    new TypeInference(symbolType).inferMetavarTypes(vars, jdgs)
}

class CollectTypesClass extends CollectTypes