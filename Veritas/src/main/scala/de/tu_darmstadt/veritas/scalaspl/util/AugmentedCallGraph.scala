package de.tu_darmstadt.veritas.scalaspl.util

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.sys.process.stringToProcess

// structural distinctions can have boolean and structural distinctions as children
// a boolean distinction can only have boolean distinctions as children
// for a function call it is possible to have every type of nodes as children
trait AugmentedCallGraph[Equation, Criteria, Expression] {
  trait Node

  case class BooleanDistinction(criteria: Criteria, resulting: Expression) extends Node

  case class StructuralDistinction(eqs: Set[Equation]) extends Node

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
    case structural: StructuralDistinction => structural.eqs
    case BooleanDistinction(_, _) =>
      getParent(distinction) match {
        case Some(StructuralDistinction(equation)) => equation
        case Some(parent) => getEquations(parent)
        case None => Set()// should not happen
      }
    case FunctionCall(_) =>
      getParent(distinction) match {
        case Some(StructuralDistinction(equation)) => equation
        case Some(parent) => getEquations(parent)
        case None => Set()// should not happen
      }
    case _ => Set()
  }

  def getExpression(distinction: Node): Expression = distinction match {
    case BooleanDistinction(_, resulting) => resulting
    case StructuralDistinction(eqs) if eqs.size == 1 => getRHSOfEquation(eqs.head)
    case FunctionCall(_) => throw new IllegalArgumentException("A function application does not represent a expression")
    case _ => throw new IllegalArgumentException("Every leaf node should be a boolean distinction or a structural distinction with exactly one equation attached.")
  }

  protected def getRHSOfEquation(eq: Equation): Expression

  // from here on: functions necessary for visualizing an augmented call graph
  protected def makeLabelFromSingleEq(eq: Equation): String

  protected def criteriaToString(c: Criteria): String

  private def normalizeHashCode(hash: Int): Int = hash & Integer.MAX_VALUE

  private def calculateNodeID(node: Node): String =
    "n" + normalizeHashCode(node.hashCode())


  private def makeDotLabelFromEqs(eqs: Set[Equation]): String =
    if (eqs.size == 1)
      makeLabelFromSingleEq(eqs.head)
    else eqs.size + " Equations"

  private def encodeNodesToDot(builder: mutable.StringBuilder): Unit = {
    for (n <- nodes) {
      builder.append(calculateNodeID(n) + " ")
      n match {
        case BooleanDistinction(c, _) =>
          builder.append("[shape=box, color=black, label=" + "\"" + criteriaToString(c) + "\"")
        case StructuralDistinction(eqs) =>
          builder.append("[shape=diamond, color=black, label=" + "\"" + makeDotLabelFromEqs(eqs) + "\"")
        case FunctionCall(name) =>
          builder.append("[shape=ellipse, color=black, label=" + name)
      }
      builder.append("];\n")
    }
  }

  private def encodeEdgesToDot(builder: mutable.StringBuilder): Unit = {
    for ((n, children) <- adjacencyList; c <- children) {
      builder.append(calculateNodeID(n) + " -> " + calculateNodeID(c) + ";\n")
    }
  }


  private def makeDotString(): String = {
    val builder: mutable.StringBuilder = StringBuilder.newBuilder

    encodeNodesToDot(builder)
    encodeEdgesToDot(builder)

    val prefix = "digraph {\n"
    val suffix = "}"
    prefix + builder.toString + suffix
  }

  // assumes existence of the 'dot' command in the PATH
  // outputPath: File which will contain the visualization. It has to end with .png
  // or with the extension specified with the second, optional parameter ext
  def visualizeACG(outputPath: File, ext: String = "png"): Unit = {
    val dotFormatted = makeDotString()
    val dotFile = new File(outputPath.getParentFile, outputPath.getName.replace(s".$ext", ".dot"))
    dotFile.createNewFile()
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(dotFormatted)
    writer.close()
    // dot -T<fileformat> <pathtodotfile> -o<outputpath>
    val exitCode = s"dot -T$ext ${dotFile.getAbsolutePath} -o${outputPath.getAbsolutePath}".!
    if (exitCode != 0)
      throw new RuntimeException("Augmented Call Graph could not be visualized. This could be caused by the non-existence of the dot command.")
  }

}
