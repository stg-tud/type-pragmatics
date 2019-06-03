package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.MetaVar

import scala.sys.process.stringToProcess

/** The provability status of refinement nodes can be one of the following:
  *  - Unknown(): the oracle has not been consulted yet
  *  - Inconclusive(): the oracle has been consulted and did not find a counterexample
  *  - DirectlyDisproved(): the oracle has been consulted and found a counterexample
  *  - IndirectlyDisproved(): the node was disproved by falsity propagation
  */
sealed trait ProvabilityStatus
case class Unknown() extends ProvabilityStatus
case class Inconclusive() extends ProvabilityStatus
case class DirectlyDisproved() extends ProvabilityStatus
case class IndirectlyDisproved() extends ProvabilityStatus

/** A node of a refinement graph. */
class RefinementNode(val annotatedLemma: AnnotatedLemma) {
  private var _parents: Seq[RefinementNode] = Seq()
  private var _children: Map[Refinement, RefinementNode] = Map()
  var provabilityStatus: ProvabilityStatus = Unknown()
  var open: Boolean = true
  var selected: Boolean = false

  def lemma: Lemma = annotatedLemma.lemma
  def constrainedVariables: Set[MetaVar] = annotatedLemma.constrainedVariables
  def postVariables: Set[MetaVar] = annotatedLemma.postVariables
  def preVariables: Set[MetaVar] = lemma.boundVariables -- postVariables

  def addChild(child: RefinementNode, refinement: Refinement): Unit = {
    _children += refinement -> child
    child.addParent(this)
  }

  def addParent(parent: RefinementNode): Unit = {
    _parents = _parents :+ parent
  }

  def parents: Seq[RefinementNode] = _parents
  def children: Seq[RefinementNode] = _children.values.toSeq
  def refinedChildren: Map[Refinement, RefinementNode] = _children
  def descendants: Set[RefinementNode] = children.toSet ++ children.flatMap(_.descendants)
  def ancestors: Set[RefinementNode] = (parents ++ parents.flatMap(_.ancestors)).toSet

  /** Create a GraphViz representation of the current node and write it to
    * a StringBuilder. Use the given ID for the node. If `detailed` is true,
    * the node label contains metainformation. */
  def makeDotString(sb: StringBuilder, nodeID: String, detailed: Boolean = true): Unit = {
    val color = provabilityStatus match {
      case _ if selected => "green"
      case Unknown() => "gray"
      case DirectlyDisproved() => "red"
      case IndirectlyDisproved() => "magenta"
      case Inconclusive() => "white"
    }
    val notConstrained = lemma.boundVariables -- constrainedVariables

    val label = if(detailed) {
      ("\"" + lemma.toString.replace("\n", "\\n")
        + s"\\n$provabilityStatus\\nopen=$open\\n"
        + s"constrained=$constrainedVariables\\nnot constrained=$notConstrained\\npost=$postVariables" + "\"")
    } else {
      "\"\""
    }
    sb.append(nodeID + s" [shape=box, label=$label, fillcolor=$color, style=filled];\n")
  }

  def select(): Unit = {
    selected = true
  }
}

class RefinementGraph(problem: Problem, val root: RefinementNode) {
  def nodes: Set[RefinementNode] = Set(root) ++ root.descendants
  def openNodes: Set[RefinementNode] = nodes.filter(_.open)
  def selectedNodes: Set[RefinementNode] = nodes.filter(_.selected)

  /** Generate a unique node ID for the given node */
  private def calculateNodeID(node: RefinementNode): String = {
    "n" + (node.hashCode() & Integer.MAX_VALUE)
  }

  /** Find a node whose lemma is \approx-equivalent to `lemma`. Return
    * None if there is no such node. */
  def findLemma(lemma: Lemma): Option[RefinementNode] = {
    nodes.find(node => LemmaEquivalence.isEquivalent(node.lemma, lemma))
  }

  /** Find all nodes with provability status `status`. */
  def collectNodes(status: ProvabilityStatus): Set[RefinementNode] = {
    nodes.filter(_.provabilityStatus == status)
  }

  /** Generate a GraphViz representation of the refinement graph and
    * return it as a string. If `defailed` is true, the node labels
    * contain metainformation.
    */
  def makeDotString(detailed: Boolean = true): String = {
    val builder = StringBuilder.newBuilder
    for(node <- nodes) {
      node.makeDotString(builder, calculateNodeID(node), detailed)
    }
    builder.append("\n")
    for(node <- nodes; (refinement, child) <- node.refinedChildren) {
      val label = if(detailed) refinement else ""
      builder.append(s"${calculateNodeID(node)} -> ${calculateNodeID(child)} [label=" + "\"" + label + "\"];\n")
    }
    "digraph {\ngraph [fontsize = 8];\n" + builder.toString() + "}"
  }

  /** Write the refinement graph to the GraphViz dot file at `dotFile`.
    * If `detailed` is true, the node labels contain metainformation. */
  def visualize(dotFile: File, detailed: Boolean = true): Unit = {
    val dotFormatted = makeDotString(detailed)
    dotFile.createNewFile()
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(dotFormatted)
    writer.close()
  }

  /** Refine a given node with the given refinement. If the refined
    * lemma already exists in the refinement graph, add a refinement
    * edge and return the node. If the refined lemma does not exist
    * in the refinement graph, add a refinement node along with an edge
    * and return the node.
    */
  def refine(node: RefinementNode, refinement: Refinement): RefinementNode = {
    AnnotatedLemma.refine(problem, node.annotatedLemma, refinement) match {
      case None => // refinement is undefined
        node
      case Some(refinedAnnotated@AnnotatedLemma(refinedLemma, _, _)) =>
        findLemma(refinedLemma) match {
          case Some(otherNode) =>
            // refined lemma already exists in the graph:
            // add a refinement edge
            require(refinedAnnotated equivalent otherNode.annotatedLemma)
            node.addChild(otherNode, refinement)
            otherNode
          case None =>
            // refined lemma does not exist in the graph:
            // add a refinement node and edge
            val child = new RefinementNode(refinedAnnotated)
            node.addChild(child, refinement)
            child
        }
    }
  }

  /** Set the provability status of `node` to DirectlyDisproved(),
    * set the provability status of all its ancestors to `IndirectlyDisproved()`.
    */
  def setDisprovedStatusRecursively(node: RefinementNode): Unit = {
    node.provabilityStatus = DirectlyDisproved()
    for(ancestor <- node.ancestors) {
      ancestor.provabilityStatus = IndirectlyDisproved()
    }
  }
}
