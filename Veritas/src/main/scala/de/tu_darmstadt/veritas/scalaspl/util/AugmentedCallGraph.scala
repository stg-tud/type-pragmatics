package de.tu_darmstadt.veritas.scalaspl.util

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.sys.process.stringToProcess

// structural distinctions can have boolean and structural distinctions as children
// a boolean distinction can only have boolean distinctions as children
// for a function call it is possible to have every type of nodes as children
// added a certain redundancy to each node in the graph: contains now also redundant "position information"
// which would allow to reconstruct a graph from its nodes only, and notably to control the correctness of a graph
trait AugmentedCallGraph[Equation, Criteria, Expression] {
  val toplevel_fun: String //name of function for which the augmented call graph is generated

  trait Node

  //for uniqueness of nodes: also save the number of the function equation in which the distinction appears (starting from 0)
  //as well as the inner nesting level (in case of nested ifs); first level is 0
  case class BooleanDistinction(funeq_num: Int, inner_nesting_level: Int, criteria: Criteria, resulting: Expression) extends Node

  //structural distinctions have to preserve the ordering of equations in function definition!
  //also save the index number of the top-level equation (for controlling purposes, and to be really sure to preserve the correct order)
  //arg_pos: argument position (in top-level function call) for which we distinguish the expressions in arg_exp - top-level arg_exp is a generic function call to the top-level function
  //arg_pos can be None if no further distinction happens (then numbered_eqs has to have only a single element)
  //arg_exp further down may be generic constructor calls (generic = fresh variable names for all arguments)
  case class StructuralDistinction(arg_pos: Option[Seq[Int]], arg_exp: Expression, numbered_eqs: Seq[(Int, Equation)]) extends Node

  //also save the index number of the top-level equation (for controlling purposes, and to be really sure to preserve the correct order)
  //for uniqueness of nodes: also save the number of the function equation in which the distinction appears (starting from 0)
  //as well as the inner nesting level (in case of nested ifs); first level is 0
  case class FunctionCall(funeq_num: Int, inner_nesting_level: Int, name: String) extends Node

  //maybe use a ListSet for children list
  private val adjacencyList: mutable.Map[Node, Seq[Node]] = mutable.Map()
  private val _sdroots = ListBuffer[StructuralDistinction]()

  // roots of augmented call graphs can only ever be StructuralDistinction nodes (by design)
  def addStructuralDistinctionRoot(snode: StructuralDistinction): Unit = {
    adjacencyList(snode) = Seq()
    _sdroots += snode
  }

  def addChild(parent: Node, child: Node): Unit = {
    if (adjacencyList.contains(parent)) {
      //prevent multi-edges! we never want them!
      if (adjacencyList(parent).contains(child))
      //for debugging purposes
      {
        //println(s"Warning: Trying to add a second edge from $parent to $child")
      }
      else {
        val children = adjacencyList(parent)
        adjacencyList(parent) = children :+ child
        if (!adjacencyList.contains(child))
          adjacencyList(child) = Seq()
      }
    } else {
      adjacencyList(parent) = Seq(child)
    }
  }

  def getOutgoing(distinction: Node): Seq[Node] =
    adjacencyList(distinction).toSeq

  def getParents(distinction: Node): Seq[Node] = {
    adjacencyList.filter { case (_, children) =>
      children.contains(distinction)
    }.keys.toSeq
  }

  def nodes: Seq[Node] = (adjacencyList.keys.toSeq ++ adjacencyList.flatMap(_._2)).distinct

  def sdroots: Set[StructuralDistinction] = _sdroots.toSet

  def leaves: Set[Node] = adjacencyList.filter(_._2.isEmpty).keys.toSet

  //only call on leaves?
  def getExpression(distinction: Node): Expression = distinction match {
    case BooleanDistinction(_, _, _, resulting) => resulting
    case StructuralDistinction(_, _, eqs) if eqs.size == 1 => getRHSOfEquation(eqs.head._2)
    case FunctionCall(_, _, _) => throw new IllegalArgumentException("A function application does not represent a expression")
    case _ => throw new IllegalArgumentException("Every leaf node should be a boolean distinction or a structural distinction with exactly one equation attached.")
  }

  def getFCParents(n: Node): Seq[FunctionCall] = {
    for (n <- getParents(n) if n.isInstanceOf[FunctionCall]) yield n.asInstanceOf[FunctionCall]
  }

  def getVariableName(mv: Expression): String

  def getVarExpAtDistarg_pos(arglist: Seq[Expression], distposlist: Seq[Int]): Expression

  protected def getRHSOfEquation(eq: Equation): Expression

  // from here on: functions necessary for visualizing an augmented call graph
  protected def makeLabelFromSingleEq(eq: Equation): String

  protected def criteriaToString(c: Criteria): String

  protected def expressionToString(expression: Expression): String

  private def normalizeHashCode(hash: Int): Int = hash & Integer.MAX_VALUE

  private def calculateNodeID(node: Node): String =
    "n" + normalizeHashCode(node.hashCode())


  private def makeDotLabelFromEqs(eqs: Seq[(Int, Equation)]): String =
    if (eqs.size == 1)
      makeLabelFromSingleEq(eqs.head._2)
    else "Equations: " + eqs.map(_._1).mkString(",")

  private def encodeNodesToDot(builder: mutable.StringBuilder): Unit = {
    for (n <- nodes) {
      builder.append(calculateNodeID(n) + " ")
      n match {
        case BooleanDistinction(feq, inner, c, _) =>
          builder.append("[shape=box, color=black, label=" + "\"" + feq + ", " + inner + ": " + criteriaToString(c) + "\"")
        case StructuralDistinction(argpos, argexp, eqs) =>
          builder.append("[shape=diamond, color=black, label=" + "\"" + argpos + ", " + expressionToString(argexp) + ": " + makeDotLabelFromEqs(eqs) + "\"")
        case FunctionCall(feq, inner, name) =>
          builder.append("[shape=ellipse, color=black, label=" + "\"" + feq + ", " + inner + ": " + name + "\"")
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
