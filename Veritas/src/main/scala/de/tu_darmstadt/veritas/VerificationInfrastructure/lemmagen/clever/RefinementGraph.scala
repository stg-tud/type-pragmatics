package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.MetaVar

import scala.sys.process.stringToProcess

sealed trait ProvabilityStatus
case class Unknown() extends ProvabilityStatus
case class Inconclusive() extends ProvabilityStatus
case class DirectlyDisproved() extends ProvabilityStatus
case class IndirectlyDisproved() extends ProvabilityStatus

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
  def leaves: Set[RefinementNode] =
    if(children.isEmpty) {
      Set(this)
    } else {
      children.flatMap(_.leaves).toSet
    }
  def ancestors: Set[RefinementNode] = (parents ++ parents.flatMap(_.ancestors)).toSet

  def makeDotString(sb: StringBuilder, nodeID: String): Unit = {
    val color = provabilityStatus match {
      case Unknown() => "gray"
      case DirectlyDisproved() => "red"
      case IndirectlyDisproved() => "magenta"
      case Inconclusive() if selected => "green"
      case Inconclusive() => "white"
    }
    val notConstrained = lemma.boundVariables -- constrainedVariables

    val label = ("\"" + lemma.toString.replace("\n", "\\n")
                + s"\\n$provabilityStatus\\nopen=$open\\n"
                + s"constrained=$constrainedVariables\\nnot constrained=$notConstrained\\npost=$postVariables" + "\"")
    sb.append(nodeID + s" [shape=box, label=$label, fillcolor=$color, style=filled];\n")
  }
}

class RefinementGraph(problem: Problem, root: RefinementNode) {
  def nodes: Set[RefinementNode] = Set(root) ++ root.descendants
  def openNodes: Set[RefinementNode] = nodes.filter(_.open)
  def leaves: Set[RefinementNode] = root.leaves

  private def calculateNodeID(node: RefinementNode): String = {
    "n" + (node.hashCode() & Integer.MAX_VALUE)
  }

  def findLemma(lemma: Lemma): Option[RefinementNode] = {
    nodes.find(node => LemmaEquivalence.isEquivalent(node.lemma, lemma))
  }

  def collectNodes(status: ProvabilityStatus): Set[RefinementNode] = {
    nodes.filter(_.provabilityStatus == status)
  }

  def makeDotString(): String = {
    val builder = StringBuilder.newBuilder
    for(node <- nodes) {
      node.makeDotString(builder, calculateNodeID(node))
    }
    builder.append("\n")
    for(node <- nodes; (refinement, child) <- node.refinedChildren) {
      builder.append(s"${calculateNodeID(node)} -> ${calculateNodeID(child)} [label=" + "\"" + refinement + "\"];\n")
    }
    "digraph {\ngraph [fontsize = 8];\n" + builder.toString() + "}"
  }

  def visualize(outputPath: File, ext: String = "png"): Unit = {
    val dotFormatted = makeDotString()
    val dotFile = new File(outputPath.getParentFile, outputPath.getName.replace(s".$ext", ".dot"))
    dotFile.createNewFile()
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(dotFormatted)
    writer.close()
    // dot -T<fileformat> <pathtodotfile> -o<outputpath>
    val exitCode = s"dot -T$ext ${dotFile.getAbsolutePath} -o${outputPath.getAbsolutePath}".!
    if (exitCode != 0)
      throw new RuntimeException("Refinement Graph could not be visualized. This could be caused by the non-existence of the dot command.")
  }

  def refine(node: RefinementNode, refinement: Refinement): RefinementNode = {
    AnnotatedLemma.refine(problem, node.annotatedLemma, refinement) match {
      case None => node
      case Some(refinedAnnotated@AnnotatedLemma(refinedLemma, _, _)) =>
        findLemma(refinedLemma) match {
          case Some(otherNode) =>
            require(refinedAnnotated equivalent otherNode.annotatedLemma)
            node.addChild(otherNode, refinement)
            otherNode
          case None =>
            val child = new RefinementNode(refinedAnnotated)
            node.addChild(child, refinement)
            child
        }
    }
  }

  def setDisprovedStatusRecursively(node: RefinementNode): Unit = {
    node.provabilityStatus = DirectlyDisproved()
    for(ancestor <- node.ancestors) {
      ancestor.provabilityStatus = IndirectlyDisproved() // TODO: overwriting the status here
    }
  }
}
