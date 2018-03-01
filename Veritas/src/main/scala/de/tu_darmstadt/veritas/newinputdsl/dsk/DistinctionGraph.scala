package de.tu_darmstadt.veritas.newinputdsl.dsk

import java.util.NoSuchElementException

import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// a boolean distinction can only have boolean distinctions as children
// equation distinctions can have boolean and equation distinctions as children
// TODO how does a function call play into this?
trait DistinctionDAG[Equation, Criteria, Expression] {
  trait Node

  case object Root extends Node

  case class BooleanDistinction(criteria: Criteria, resulting: Expression) extends Node

  case class EquationDistinction(eqs: Set[Equation]) extends Node

  case class FunctionCall(name: String, args: Seq[Expression]) extends Node

  private val adjacencyList: mutable.Map[Node, Set[Node]] = mutable.Map()
  // add root to list
  adjacencyList(Root) = Set()

  def addChild(parent: Node, child: Node): Unit = {
    if (adjacencyList.contains(parent)) {
      val children = adjacencyList(parent)
      adjacencyList(parent) = children + child
      if (!adjacencyList.contains(child))
        adjacencyList(child) = Set()
    } else throw new NoSuchElementException(s"$parent was not found")
  }

  def addChildren(parent: Node, children: Seq[Node]): Unit = {
    children.foreach { addChild(parent, _) }
  }

  def getNeighbours(distinction: Node): Seq[Node] =
    adjacencyList(distinction).toSeq

  def getParent(distinction: Node): Option[Node] = {
    adjacencyList.filter { case (parent, children) =>
        children.contains(distinction)
    }.keys.headOption
  }

  def distinctions: Set[Node] = adjacencyList.keys.toSet

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
    case FunctionCall(name, args) => getFunctionCall(name, args)
    case _ => throw new IllegalArgumentException("Every leave node should be a boolean distinction or a structural distinction with exactly one equation attached.")
  }

  protected def getRHSOfEquation(eq: Equation): Expression

  protected def getFunctionCall(name: String, args: Seq[Expression]): Expression
}

trait DistinctionDAGBuilder[FunDef, Eq, Criteria, Exp, Graph <: DistinctionDAG[Eq, Criteria, Exp]] {

  def translate(funDef: FunDef)(dag: Graph): DistinctionDAG[Eq, Criteria, Exp] = {
    val groupedEquations = groupFunctionEquations(getEquationsOfDefintion(funDef))
    val maxLevel = groupedEquations.map(_._1).max
    val proccesedEquations: ListBuffer[(Int, dag.EquationDistinction)] = ListBuffer()
    groupedEquations.foreach(println)

    // Idea: group is parent of other group if it is a superset of it and has a lower lvl
    def buildChildrenBasedOnPattern(level: Int): Unit = {
      val currentLevelEquations = groupedEquations.filter(_._1 == level)
      currentLevelEquations.foreach { case (lvl, eqs) =>
        val parentCandidates = proccesedEquations.filter(x => eqs.forall(x._2.eqs.contains))
        // direct parent is the smallest superset of the eqs
        val parent =
          if (parentCandidates.isEmpty)
            (1, dag.Root)
          else
            parentCandidates.minBy(_._2.eqs.size)
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
    def buildChildrenBasedOnFunctionExp(node: dag.Node): Unit = {
      val booleanCriterias = getDistinctionByIfExpression(dag.getExpression(node))
      println(booleanCriterias)
      booleanCriterias.foreach { case (criteria, resultingExp) =>
        val child = dag.BooleanDistinction(criteria, resultingExp)
        dag.addChild(node, child)
        buildChildrenBasedOnFunctionExp(child)
      }
    }
    structuralLeaves.foreach { buildChildrenBasedOnFunctionExp(_) }
    dag
  }

  protected def getEquationsOfDefintion(funDef: FunDef): Seq[Eq]

  protected def groupFunctionEquations(eqs: Seq[Eq], positionToWatch: Int = 0): Seq[(Int, Set[Eq])]

  protected def getDistinctionByIfExpression(exp: Exp): Map[Criteria, Exp]
}


case class VeritasDistinctionDAG() extends DistinctionDAG[FunctionEq, FunctionExp, FunctionExpMeta] {
  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right

  override protected def getFunctionCall(name: String, args: Seq[FunctionExpMeta]): FunctionExpMeta =
    FunctionExpApp(name, args)
}

class VeritasDistinctionDAGBuilder extends DistinctionDAGBuilder[FunctionDef, FunctionEq, FunctionExp , FunctionExpMeta, VeritasDistinctionDAG] {

  protected def groupFunctionEquations(eqs: Seq[FunctionEq], positionToWatch: Int = 0): Seq[(Int, Set[FunctionEq])] = {
    val patternStrings = eqs.map { eq => (createStringFromPattern(eq.patterns(positionToWatch)), eq)}.sortBy(_._1)
    val grouped = mutable.Map[String, Set[FunctionEq]]()

    // group based on prefixes
    patternStrings.foreach { case (string, eq) =>
        val prefixes = VeritasDistinctionDAGBuilder.substringByChar(string, '_')
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
        val moreGeneral = VeritasDistinctionDAGBuilder.substringByChar(prefix, '_')
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
    case FunctionExpLet(name, namedExpr, in) =>
      getDistinctionByIfExpression(namedExpr) ++ getDistinctionByIfExpression(in)
    case _ => Map()
  }

  override protected def getEquationsOfDefintion(funDef: FunctionDef): Seq[FunctionEq] = funDef.eqn
}

object VeritasDistinctionDAGBuilder {
  def substringByChar(str: String, sign: Char): Seq[String] = {
    var indices = Seq[Int]()
    for { (char, index) <- str.zipWithIndex
          if char == sign
    } yield str.substring(0, index)
  }
}
