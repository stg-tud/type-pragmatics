package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.GoalsWithStrategy
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpBiImpl
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpNot
import de.tu_darmstadt.veritas.backend.veritas.ReduceJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpLet
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.veritas.TypingJudgmentSimple
import de.tu_darmstadt.veritas.backend.veritas.ExistsJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpIf
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpNeq
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.NotJudgment
import de.tu_darmstadt.veritas.backend.veritas.TypingJudgment
import de.tu_darmstadt.veritas.backend.veritas.TypingRule
import de.tu_darmstadt.veritas.backend.veritas.OrJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpEq
import de.tu_darmstadt.veritas.backend.veritas.ForallJudgment
import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpOr
import de.tu_darmstadt.veritas.backend.veritas.TypingRuleJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpAnd
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExp
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpVar
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpTrue
import de.tu_darmstadt.veritas.backend.veritas.MetaVar
import de.tu_darmstadt.veritas.backend.veritas.Sorts
import de.tu_darmstadt.veritas.backend.veritas.Consts
import de.tu_darmstadt.veritas.backend.veritas.PartialFunctions
import de.tu_darmstadt.veritas.backend.veritas.DataType
import de.tu_darmstadt.veritas.backend.veritas.Functions
import de.tu_darmstadt.veritas.backend.veritas.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.veritas.SortRef
import de.tu_darmstadt.veritas.backend.veritas.ConstDecl
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionDef
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionSig
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionEq
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionPattern
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionPatVar
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionPatApp
import de.tu_darmstadt.veritas.backend.veritas.SortDef
import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import scala.util.matching.Regex

trait InformationCollector[T] {
  
  val collectedInfo = scala.collection.mutable.Set[T]()
  
  def apply(m: Module): Set[T] = {
    m.defs.foreach (collectFromModuleDef)
    collectedInfo.toSet
  }
  
  def apply(m: Module, acc: Set[T]): Set[T] = apply(m) ++ acc

  /**
   * convenience function, can be used whenever overriding one of the trans- functions below
   * in a concrete transformation to execute the "parent transformation" first
   *
   * subtransres is the result of applying the super transformation, f the new transformation that
   * shall be applied in addition, leaving unchanged all constructs for which the f is not defined
   */
  
  def withSuper[S <: VeritasConstruct](construct: S)(supcollect: S => Unit)(f: PartialFunction[S, Unit]): Unit = {
    if (f.isDefinedAt(construct)) 
      f(construct)
    supcollect(construct)
  }
  
  def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
    case g @ Goals(_, _) => collectFromGoals(g)
    case a @ Axioms(_) => collectFromAxioms(a)
    case DataType(_, name, cs) => cs foreach collectFromDataTypeConstructor
    case Consts(cs, _) => cs foreach collectFromConstDecl
    case Sorts(s) => s foreach collectFromSortDef
    case fs @ Functions(fds) => collectFromFunctions(fs)
    case pfs @ PartialFunctions(fds) => collectFromPartialFunctions(pfs)
    case _ => {}
  }
  
  def collectFromGoals(goals: Goals): Unit = {
    goals.goals foreach collectFromTypingRule
  }
  
  def collectFromTypingRule(tr: TypingRule): Unit = {
    (tr.premises ++ tr.consequences) foreach collectFromTypingRuleJudgment
  }

  def collectFromTypingRuleJudgment(trj: TypingRuleJudgment): Unit = trj match {
    case TypingJudgment(f1, f2, f3) => Seq(f1, f2, f3) foreach collectFromFunctionExpMeta
    case TypingJudgmentSimple(f1, f2) => Seq(f1, f2) foreach collectFromFunctionExpMeta
    case FunctionExpJudgment(f) => collectFromFunctionExpMeta(f)
    case ExistsJudgment(_, jdglst) => jdglst foreach collectFromTypingRuleJudgment
    case ForallJudgment(_, jdglst) => jdglst foreach collectFromTypingRuleJudgment
    case ReduceJudgment(f1, f2) => Seq(f1, f2) foreach collectFromFunctionExpMeta
    case NotJudgment(jdg) => collectFromTypingRuleJudgment(jdg)
    case OrJudgment(orCases) => orCases.flatten foreach collectFromTypingRuleJudgment
    case _ => TransformationError("Unsupported TypingRuleJudgment!")
  }
  
  
  def collectFromFunctionExpMeta(fem: FunctionExpMeta): Unit = fem match {
    case FunctionExpAnd(left, right) => Seq(left, right) foreach collectFromFunctionExpMeta
    case FunctionExpApp(name, args) => args foreach collectFromFunctionExpMeta 
    case FunctionExpBiImpl(left, right) => Seq(left, right) foreach collectFromFunctionExpMeta
    case FunctionExpEq(f1, f2) => Seq(f1, f2) foreach collectFromFunctionExpMeta
    case FunctionExpIf(c, i, t) => Seq(c, i, t) foreach collectFromFunctionExpMeta
    case FunctionExpLet(name, f1, f2) => Seq(f1, f2) foreach collectFromFunctionExpMeta
    case FunctionExpNeq(f1, f2) => Seq(f1, f2) foreach collectFromFunctionExpMeta
    case FunctionExpNot(f) => collectFromFunctionExpMeta(f)
    case FunctionExpOr(left, right) => Seq(left, right) foreach collectFromFunctionExpMeta
    case FunctionExpVar(name) => {}
    case FunctionMeta(metaVar) => collectFromMetaVar(metaVar)
    case FunctionExpFalse => {}
    case FunctionExpTrue => {}
    case _ => TransformationError("Unsupported FunctionExpMeta!")
  }
  
  def collectFromMetaVar(metaVar: MetaVar): Unit = {}
  
  def collectFromDataType(dt: DataType): Unit = {
    dt.constrs foreach collectFromDataTypeConstructor
  }
   
  def collectFromAxioms(axioms: Axioms): Unit = {
    axioms.axioms foreach collectFromTypingRule
  }
  
  def collectFromDataTypeConstructor(dtc: DataTypeConstructor): Unit = {
    dtc.in foreach collectFromSortRef
  }
  
  def collectFromSortDef(sd: SortDef): Unit = {}
  
  def collectFromSortRef(sr: SortRef): Unit = {}
  
  def collectFromConsts(cs: Consts): Unit = {
    cs.consts foreach collectFromConstDecl
  }
  
  def collectFromConstDecl(cd: ConstDecl): Unit = {
    collectFromSortRef(cd.out)
  }
  
  def collectFromFunctions(fs: Functions): Unit = {
    fs.funcs foreach collectFromFunctionDef
  }
  
  def collectFromFunctionDef(fd: FunctionDef): Unit = {
    collectFromFunctionSig(fd.signature)
    fd.eqn foreach collectFromFunctionEq
  }
  
  def collectFromFunctionSig(fs: FunctionSig): Unit = {
    fs.in foreach collectFromSortRef
    collectFromSortRef(fs.out)
  }

  def collectFromFunctionEq(fe: FunctionEq): Unit = {
    fe.patterns foreach collectFromFunctionPattern
    collectFromFunctionExpMeta(fe.right)
  }
  
  def collectFromFunctionPattern(fp: FunctionPattern): Unit = fp match {
    case FunctionPatApp(_, args) => args foreach collectFromFunctionPattern
    case FunctionPatVar(_) => {}
  }
  
  def collectFromPartialFunctions(fs: PartialFunctions): Unit = {
    fs.funcs foreach collectFromFunctionDef
  }
}

trait ConstructorAndFunctionNameInModuleDefCollector extends InformationCollector[String] {
 
  override def collectFromFunctionExpMeta(fem: FunctionExpMeta): Unit = 
    withSuper(fem)(super.collectFromFunctionExpMeta) {
      case FunctionExpApp(name, args) => collectedInfo += name
    }
}

object ConstructorAndFunctionNameInGoalCollector extends ConstructorAndFunctionNameInModuleDefCollector {
    
  override def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
    case g @ Goals(_, _) =>collectFromGoals(g)
    case _ => {}
  }
}

case class DataTypeNameOfConstructorCollector(names: Set[String]) extends InformationCollector[String] {  
  
  override def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
    case a @ Axioms(_) => collectFromAxioms(a)
    case _ => {}
  }
  
  override def collectFromTypingRule(tr: TypingRule): Unit = {
    // someTable -> OptTable
    // ground-OptTable-someTable
    // someTable -> DIFF-noTable-someTable
    // TODO: what types of characters are allowed for datatypes
    names.foreach { n =>
      val pattern = ("""ground-([a-zA-Z][a-zA-Z0-9]*)-""" + n).r
      tr.name match {
        case pattern(datatypeName) => collectedInfo += datatypeName
        case _ => {}
      }
    }
  }
}

object InformationCollectorUtil {
  
  def matchesRegex(s: String, regex: Regex): Boolean = regex.pattern.matcher(s).matches
  
  def matchesCotr(s: String, name: String): Boolean = {
    val ground = ("""ground-""" + name + """-[a-zA-Z]+""").r
    val dom = ("""dom-""" + name).r
    val eq = ("""EQ-""" + name).r
    val diffFirst = ("""DIFF-[a-zA-Z]+-""" + name).r
    val diffSecond = ("""DIFF-""" + name + """-[a-zA-Z]+""").r
  
    matchesRegex(s, ground) || matchesRegex(s, dom) ||  matchesRegex(s, eq) || matchesRegex(s, diffFirst) || matchesRegex(s, diffSecond)
  }
  
  def matchesFunction(s: String, name: String): Boolean = {
    val function = (name + """-([0-9]+|(true|false)-INV|INV)""").r
  
    matchesRegex(s, function)
  }
}

case class ConstructorNameOfDataTypeCollector(names: Set[String]) extends InformationCollector[String] { 
  
  override def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
      case a @ Axioms(_) => collectFromAxioms(a)
      case _ => {}
    }
  
  override def collectFromTypingRule(tr: TypingRule): Unit = {
    // someTable -> OptTable
    // ground-OptTable-someTable
    // someTable -> DIFF-noTable-someTable
    // TODO: what types of characters are allowed for datatypes
    names.foreach { n =>
      val pattern = ("""ground-""" + n + """-([a-zA-Z][a-zA-Z0-9]*)""").r
      tr.name match {
        case pattern(cotrName) => collectedInfo += cotrName
        case _ => {}
      }
    }
  }
}

case class ConstructorAndFunctionNameInAxiomCollector(axioms: Set[TypingRule]) extends ConstructorAndFunctionNameInModuleDefCollector {
  
  override def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
    case a @ Axioms(_) => collectFromAxioms(a)
    case _ => {}
  }
  
  override def collectFromTypingRule(tr: TypingRule): Unit = 
    if(axioms.contains(tr))
      super.collectFromTypingRule(tr)
}

case class AxiomDefiningConstructorAndFunctionCollector(fNames: Set[String], ctorNames: Set[String]) extends InformationCollector[TypingRule] {
  
  // to speed up traversing the tree
  override def collectFromModuleDef(mdef: ModuleDef): Unit = mdef match {
    case a @ Axioms(_) => collectFromAxioms(a)
    case _ => {}
  }  
  
  override def collectFromTypingRule(tr: TypingRule): Unit =
    withSuper(tr)(super.collectFromTypingRule) {
      case TypingRule(_, _, _) => collectAxiom(tr)
    }
  
  private def collectAxiom(tr: TypingRule): Unit = {
    ctorNames.foreach { name =>
      if(InformationCollectorUtil.matchesCotr(tr.name, name))
        collectedInfo += tr
    }
  
    fNames.foreach { name =>
      if(InformationCollectorUtil.matchesCotr(tr.name, name) || InformationCollectorUtil.matchesFunction(tr.name, name))
        collectedInfo += tr
    }
  }
}

/**
 * @param depth how deep to search for names which are used by constructors and functions.
 */
case class ConstructorAndFunctionNameCollector(depth: Int) extends InformationCollector[TypingRule] {
  
  var currentDepth = 1
  
  override def apply(m: Module): Set[TypingRule] = {
    val names = ConstructorAndFunctionNameInGoalCollector(m)
    val axioms = collectAxiomsBasedOnNames(m, names)
    recurse(m, axioms)
  }
  
  private def collectAxiomsBasedOnNames(m: Module, names: Set[String]): Set[TypingRule] = {
    val dtNames = DataTypeNameOfConstructorCollector(names)(m)
    val otherCotrNames = ConstructorNameOfDataTypeCollector(dtNames)(m)
    AxiomDefiningConstructorAndFunctionCollector(names ++ otherCotrNames, dtNames)(m)
  private def recurse(m: Module, axioms: Set[TypingRule]): Set[TypingRule] =
    if (currentDepth >= depth) {
      names
      axioms
    } else {
      currentDepth += 1
      val dtNames = DataTypeNameOfConstructorCollector(names)(m)
      val otherCotrNames = ConstructorNameOfDataTypeCollector(dtNames)(m)
      val axioms: Set[TypingRule] = AxiomDefiningConstructorAndFunctionCollector(names ++ otherCotrNames, dtNames)(m)
      val newNames = ConstructorAndFunctionNameInAxiomCollector(axioms)(m)
      names ++ recurse(m, newNames)
      val newAxioms = collectAxiomsBasedOnNames(m, newNames)
      axioms ++ recurse(m, newAxioms)
    }
    
}

// For combining multiple collectors
case class InformationCollectorSeq[T](ics: InformationCollector[T]*) extends InformationCollector[T] {
  
  override def apply(m: Module): Set[T] = {
    var acc = Set[T]()
    ics.foreach { ic =>
      acc = ic(m, acc)
    }
    acc
  }
}

trait AxiomSelector {
  
  def apply(m: Module): Module = Module(m.filename, m.imports, m.defs map transformModuleDef)

  final def transformModuleDef(mdef: ModuleDef): ModuleDef = mdef match {
    case Axioms(axioms) => Axioms(axioms filter selectAxiom)
    case _ => mdef
  }

  def selectAxiom(tr: TypingRule): Boolean
}

  
}