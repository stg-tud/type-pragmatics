package de.tu_darmstadt.veritas.newinputdsl.dsk

import java.util.NoSuchElementException

import de.tu_darmstadt.veritas.backend.ast.{DataType, Module}
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// a boolean distinction can only have boolean distinctions as children
// equation distinctions can have boolean and equation distinctions as children
trait DistinctionCallDAG[Equation, Criteria, Expression] {
  trait Node

  case class BooleanDistinction(criteria: Criteria, resulting: Expression) extends Node

  case class EquationDistinction(eqs: Set[Equation]) extends Node

  case class FunctionCall(name: String) extends Node

  private val adjacencyList: mutable.Map[Node, Set[Node]] = mutable.Map()
  private val _roots = ListBuffer[Node]()

  def addRoot(node: Node): Unit = {
    adjacencyList(node) = Set()
    _roots += node
  }

  def addChild(parent: Node, child: Node): Unit = {
    if (adjacencyList.contains(parent)) {
      val children = adjacencyList(parent)
      adjacencyList(parent) = children + child
      if (!adjacencyList.contains(child))
        adjacencyList(child) = Set()
    } else {
      adjacencyList(parent) = Set(child)
    }
  }

  def getOutgoing(distinction: Node): Seq[Node] =
    adjacencyList(distinction).toSeq

  def getParent(distinction: Node): Option[Node] = {
    adjacencyList.filter { case (parent, children) =>
        children.contains(distinction)
    }.keys.headOption
  }

  def nodes: Seq[Node] = (adjacencyList.keys.toSeq ++ adjacencyList.flatMap(_._2)).distinct

  def roots: Set[Node] = _roots.toSet

  def leaves: Set[Node] = adjacencyList.filter(_._2.isEmpty).keys.toSet

  def getEquations(distinction: Node): Set[Equation] = distinction match {
    case structural: EquationDistinction => structural.eqs
    case boolean: BooleanDistinction =>
      getParent(distinction) match {
        case Some(EquationDistinction(equation)) => equation
        case Some(parent) => getEquations(parent)
        case None => Set()// should not happen
      }
    case funcCall: FunctionCall =>
      getParent(distinction) match {
        case Some(EquationDistinction(equation)) => equation
        case Some(parent) => getEquations(parent)
        case None => Set()// should not happen
      }
    case _ => Set()
  }

  def getExpression(distinction: Node): Expression = distinction match {
    case BooleanDistinction(_, resulting) => resulting
    case EquationDistinction(eqs) if eqs.size == 1 => getRHSOfEquation(eqs.head)
    case FunctionCall(name) =>
      getParent(distinction) match {
        case Some(parent) => getExpression(parent)
        case None => throw new IllegalArgumentException("Should not happend")
      }
    case _ => throw new IllegalArgumentException("Every leave node should be a boolean distinction or a structural distinction with exactly one equation attached.")
  }

  protected def getRHSOfEquation(eq: Equation): Expression
}

trait DistinctionCallDAGBuilder[FunDef, Eq, Criteria, Exp, Graph <: DistinctionCallDAG[Eq, Criteria, Exp]] {

  def translate(funDef: FunDef)(dag: Graph): DistinctionCallDAG[Eq, Criteria, Exp] = {
    val groupedEquations = groupFunctionEquations(getEquationsOfDefintion(funDef))
    val maxLevel = groupedEquations.map(_._1).max
    val proccesedEquations: ListBuffer[(Int, dag.EquationDistinction)] = ListBuffer()

    // add root with all function equations
    val root = dag.EquationDistinction(getEquationsOfDefintion(funDef).toSet)
    dag.addRoot(root)
    proccesedEquations += 1 -> root
    // Idea: group is parent of other group if it is a superset of it and has a lower lvl
    def buildChildrenBasedOnPattern(level: Int): Unit = {
      val currentLevelEquations = groupedEquations.filter(_._1 == level)
      currentLevelEquations.foreach { case (lvl, eqs) =>
        val parentCandidates = proccesedEquations.filter(x => eqs.forall(x._2.eqs.contains))
        // direct parent is the smallest superset of the eqs
        val parent = parentCandidates.minBy(_._2.eqs.size)
        val child = dag.EquationDistinction(eqs)
        dag.addChild(parent._2, child)
        proccesedEquations += lvl -> child
      }
      if (level < maxLevel)
        buildChildrenBasedOnPattern(level + 1)
    }
    buildChildrenBasedOnPattern(2)

    val leaves = dag.leaves
    // every leave at this stage has to be a structural leave
    val structuralLeaves = leaves.map(_.asInstanceOf[dag.EquationDistinction])

    def buildChildrenBasedOnFunctionExp(node: dag.Node, foundBindings: Map[String, Set[String]]): Unit = {
      val exp = dag.getExpression(node)
      val nestedFunctionApps = getNestedFunctionApplications(exp)
      nestedFunctionApps.foreach { funcNames =>
        val outerNode = dag.FunctionCall(funcNames.head)
        dag.addChild(outerNode, node)
        if (funcNames.size == 2) {
          val innerNode = dag.FunctionCall(funcNames(1))
          dag.addChild(innerNode, outerNode)
        }
      }

      // maps from function name to passed varrefs
      val varRefdByFunction: Map[String, Set[String]] = getVarRefWithinFunctionApp(exp)
      // maps from var name to functions it used to create the binding
      val bindings: Map[String, Set[String]] = foundBindings ++ getResultBindings(exp)
      varRefdByFunction.foreach { case (funName, refNames) =>
        val funCall = dag.FunctionCall(funName)
        // because function was called in within a let or a the cond of if we link node to funCall
        dag.addChild(funCall, node)

        bindings.foreach { case (bindingName, funcApps) =>
          // binding was used in a funApp
          if (refNames.contains(bindingName)) {
            funcApps.foreach { funcName =>
              val parent = dag.FunctionCall(funcName)
              dag.addChild(parent, funCall)
            }
          }
        }
      }
      // create children based on if condition
      val conditionalBranches = getDistinctionByIfExpression(dag.getExpression(node))
      conditionalBranches.foreach { case (condition, branch) =>
        val branchNode = dag.BooleanDistinction(condition, branch)
        dag.addChild(node, branchNode)
        buildChildrenBasedOnFunctionExp(branchNode, bindings)
      }
    }

    //
    structuralLeaves.foreach { leave =>
      buildChildrenBasedOnFunctionExp(leave, Map())
    }
    dag
  }

  protected def getEquationsOfDefintion(funDef: FunDef): Seq[Eq]

  protected def groupFunctionEquations(eqs: Seq[Eq], positionToWatch: Int = 0): Seq[(Int, Set[Eq])]

  protected def getDistinctionByIfExpression(exp: Exp): Map[Criteria, Exp]

  protected def getFunctionApplication(exp: Exp): Option[String]
  // list of lists where inner list has one element or two elements
  // first element is outer function application, second is a possible nested function application
  protected def getNestedFunctionApplications(exp: Exp): Seq[Seq[String]]
  protected def getVarRefWithinFunctionApp(exp: Exp): Map[String, Set[String]]
  protected def getResultBindings(exp: Exp): Map[String, Set[String]]
}


case class VeritasDistinctionCallDAG() extends DistinctionCallDAG[FunctionEq, FunctionExp, FunctionExpMeta] {
  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right
}

class VeritasDistinctionCallDAGBuilder(spec: Module) extends DistinctionCallDAGBuilder[FunctionDef, FunctionEq, FunctionExp , FunctionExpMeta, VeritasDistinctionCallDAG] {

  private val ctorNames = ListBuffer[String]()

  override def translate(funDef: FunctionDef)(dag: VeritasDistinctionCallDAG): DistinctionCallDAG[FunctionEq, FunctionExp, FunctionExpMeta] = {
    spec.defs.foreach {
      case DataType(_, _, ctors) =>
        ctorNames ++= ctors.map(_.name)
      case _ =>
    }
    super.translate(funDef)(dag)
  }

  protected def groupFunctionEquations(eqs: Seq[FunctionEq], positionToWatch: Int = 0): Seq[(Int, Set[FunctionEq])] = {
    val patternStrings = eqs.map { eq => (createStringFromPattern(eq.patterns(positionToWatch)), eq)}.sortBy(_._1)
    val grouped = mutable.Map[String, Set[FunctionEq]]()

    // group based on prefixes
    patternStrings.foreach { case (string, eq) =>
        val prefixes = VeritasDistinctionCallDAGBuilder.substringByChar(string, '_')
        for ((prefix, i) <- prefixes.zipWithIndex) {
          if (grouped.contains(prefix)) {
            grouped(prefix) = grouped(prefix) + eq
          } else {
            grouped(prefix) = Set(eq)
          }
        }
    }

    patternStrings.foreach { case (prefix, eq) =>
        if (!grouped.contains(prefix))
          grouped(prefix) = Set(eq)
    }

    // remove groupings which are more specifc but have the same elements
    val cleanedUpGroupings = grouped.filter { case (prefix, eqs) =>
        val moreGeneral = VeritasDistinctionCallDAGBuilder.substringByChar(prefix, '_')
        moreGeneral.forall { pr2 =>
          val moreGeneralEqs = grouped(pr2)
          moreGeneralEqs != eqs
        }
    }
    cleanedUpGroupings.toSeq.sortBy(_._1).map(x => (x._1.split("_").size, x._2))
  }

  protected def createStringFromPattern(pattern: FunctionPattern): String = pattern match {
    case FunctionPatApp(name, args) =>
      name + "_" + args.map(createStringFromPattern).mkString("%")
    case FunctionPatVar(name) => name
  }

  // maps from condition to branch
  override protected def getDistinctionByIfExpression(exp: FunctionExpMeta): Map[FunctionExp, FunctionExpMeta] = exp match {
    case FunctionExpIf(cond, thn, els) => Map() + (cond -> thn) + (FunctionExpNot(cond) -> els)
    case FunctionExpLet(_, namedExpr, in) =>
      getDistinctionByIfExpression(namedExpr) ++ getDistinctionByIfExpression(in)
    case _ => Map()
  }

  // TODO currently only supports sinlge funcapp
  override protected def getResultBindings(exp: FunctionExpMeta): Map[String, Set[String]] = exp match {
    case FunctionExpLet(name, FunctionExpApp(funcName, _), in) =>
      Map() + (name -> Set(funcName)) ++ getResultBindings(in)
    case _ => Map()
  }

  override protected def getNestedFunctionApplications(exp: FunctionExpMeta): Seq[Seq[String]] = exp match {
    case FunctionExpApp(name, args) =>
      val inner =
        if (!ctorNames.contains(name))
          Seq(Seq(name)) ++ args.flatMap(getFunctionApplication).map { Seq(name, _) }
        else Seq()
      inner ++ args.flatMap(getNestedFunctionApplications)
    case FunctionExpLet(_, named, in) => getNestedFunctionApplications(named) ++ getNestedFunctionApplications(in)
    case FunctionExpIf(cond, _, _)  => getNestedFunctionApplications(cond)
    case _ => Seq()
  }

  override protected def getFunctionApplication(exp: FunctionExpMeta): Option[String] = exp match {
    case FunctionExpApp(name, args) =>
      if (!ctorNames.contains(name)) Some(name)
      else None
    case _ => None
  }

  override protected def getEquationsOfDefintion(funDef: FunctionDef): Seq[FunctionEq] = funDef.eqn

  override protected def getVarRefWithinFunctionApp(exp: FunctionExpMeta): Map[String, Set[String]] = exp match {
    case FunctionExpApp(name, args) =>
      val refs =
        if (!ctorNames.contains(name))
          Map(name -> args.flatMap(getVarRefs).toSet)
        else Map()
      refs ++ args.flatMap(getVarRefWithinFunctionApp)
    case FunctionExpIf(cond, _, _) => getVarRefWithinFunctionApp(cond)
    case FunctionExpLet(_, namedExp, in) =>
      updateMap(getVarRefWithinFunctionApp(namedExp), getVarRefWithinFunctionApp(in))
    case FunctionExpNot(f) => getVarRefWithinFunctionApp(f)
    case FunctionExpEq(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpNeq(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpAnd(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpOr(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpBiImpl(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case _ => Map()
  }

  private def getVarRefs(exp:FunctionExpMeta): Option[String] = exp match {
    case FunctionExpVar(name) => Some(name)
    case _ => None
  }

  private def updateMap[K, V](prev: Map[K, Set[V]], now: Map[K, Set[V]]): Map[K, Set[V]] = {
    val result = mutable.Map() ++ prev
    now.foreach { case (key, vals) =>
      if (result.contains(key))
        result(key) = result(key) ++ vals
      else
        result(key) = vals
    }
    Map() ++ result
  }
}

object VeritasDistinctionCallDAGBuilder {
  def substringByChar(str: String, sign: Char): Seq[String] = {
    var indices = Seq[Int]()
    for { (char, index) <- str.zipWithIndex
          if char == sign
    } yield str.substring(0, index)
  }
}
