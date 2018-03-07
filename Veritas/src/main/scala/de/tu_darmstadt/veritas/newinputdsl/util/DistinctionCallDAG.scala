package de.tu_darmstadt.veritas.newinputdsl.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// equation distinctions can have boolean and equation distinctions as children
// a boolean distinction can only have boolean distinctions as children
// for a function call it is possible to have every type of nodes as children
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
    adjacencyList.filter { case (_, children) =>
      children.contains(distinction)
    }.keys.headOption
  }

  def nodes: Seq[Node] = (adjacencyList.keys.toSeq ++ adjacencyList.flatMap(_._2)).distinct

  def roots: Set[Node] = _roots.toSet

  def leaves: Set[Node] = adjacencyList.filter(_._2.isEmpty).keys.toSet

  def getEquations(distinction: Node): Set[Equation] = distinction match {
    case structural: EquationDistinction => structural.eqs
    case BooleanDistinction(_, _) =>
      getParent(distinction) match {
        case Some(EquationDistinction(equation)) => equation
        case Some(parent) => getEquations(parent)
        case None => Set()// should not happen
      }
    case FunctionCall(_) =>
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
    case FunctionCall(_) =>
      getParent(distinction) match {
        case Some(parent) => getExpression(parent)
        case None => throw new IllegalArgumentException("Should not happend")
      }
    case _ => throw new IllegalArgumentException("Every leave node should be a boolean distinction or a structural distinction with exactly one equation attached.")
  }

  protected def getRHSOfEquation(eq: Equation): Expression
}