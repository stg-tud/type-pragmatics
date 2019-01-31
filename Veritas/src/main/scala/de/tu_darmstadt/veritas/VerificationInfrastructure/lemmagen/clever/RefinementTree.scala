package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._

import scala.sys.process.stringToProcess

sealed trait OracleStatus
case class Unknown() extends OracleStatus
case class Inconclusive() extends OracleStatus
case class Incorrect() extends OracleStatus
case class Unexpected(answer: (Oracle.Answer, Oracle.Answer)) extends OracleStatus

sealed trait RefinementStatus
case class ShouldNotRefine() extends RefinementStatus
case class ShouldRefine() extends RefinementStatus
case class Refined() extends RefinementStatus

class RefinementNode(val tree: RefinementTree, val lemma: Lemma, val refinement: Option[Refinement]) {
  private var _children: Seq[RefinementNode] = Seq()
  var oracleStatus: OracleStatus = Unknown()
  var refinementStatus: RefinementStatus = ShouldNotRefine()

  def addChild(child: RefinementNode): Unit = {
    _children :+= child
  }

  def children: Seq[RefinementNode] = _children
  def descendants: Set[RefinementNode] = children.toSet ++ children.flatMap(_.descendants)
  def leaves: Set[RefinementNode] =
    if(children.isEmpty) {
      Set(this)
    } else {
      children.flatMap(_.leaves).toSet
    }

  def refine(problem: Problem, refinement: Refinement): RefinementNode = {
    refinement.refine(problem, lemma) match {
      case None => this
      case Some(refinedLemma) =>
        tree.findLemma(refinedLemma) match {
          case Some(node) => {
            addChild(node)
            node
          }
          case None =>
            val child = new RefinementNode(tree, refinedLemma, Some(refinement))
            addChild(child)
            child
        }
    }
  }

  def findRefinement(refinement: Refinement): Option[RefinementNode] = children.find(_.refinement.exists(r => r == refinement))

  def makeDotString(sb: StringBuilder, nodeID: String): Unit = {
    /*val color = status match {
      case Unknown() => "gray"
      case ProvablyFalse() => "red"
      case Inconclusive() => "black"
    }*/
    val color = "black"
    val label = "\"" + lemma.toString.replace("\n", "\\n") + s"\\n$oracleStatus\n$refinementStatus" + "\""
    sb.append(nodeID + s" [shape=box, label=$label, color=$color];\n")
  }
}

class RefinementTree(rootLemma: Lemma) {
  val root = new RefinementNode(this, rootLemma, None)
  def nodes: Set[RefinementNode] = Set(root) ++ root.descendants
  def leaves: Set[RefinementNode] = root.leaves

  private def calculateNodeID(node: RefinementNode): String = {
    "n" + (node.hashCode() & Integer.MAX_VALUE)
  }

  def findLemma(lemma: Lemma): Option[RefinementNode] = {
    nodes.find(node => LemmaEquivalence.isEquivalent(node.lemma, lemma))
  }

  def collectNodes(status: OracleStatus): Set[RefinementNode] = {
    nodes.filter(_.oracleStatus == status)
  }

  def collectNodes(status: RefinementStatus): Set[RefinementNode] = {
    nodes.filter(_.refinementStatus == status)
  }

  def makeDotString(): String = {
    val builder = StringBuilder.newBuilder
    for(node <- nodes) {
      node.makeDotString(builder, calculateNodeID(node))
    }
    builder.append("\n")
    for(node <- nodes; child <- node.children) {
      builder.append(calculateNodeID(node) + " -> " + calculateNodeID(child) + ";\n")
    }
    "digraph {\ngraph [fontsize = 8];\n" + builder.toString() + "}"
  }

  def visualizeRT(outputPath: File, ext: String = "png"): Unit = {
    val dotFormatted = makeDotString()
    val dotFile = new File(outputPath.getParentFile, outputPath.getName.replace(s".$ext", ".dot"))
    dotFile.createNewFile()
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(dotFormatted)
    writer.close()
    // dot -T<fileformat> <pathtodotfile> -o<outputpath>
    val exitCode = s"dot -T$ext ${dotFile.getAbsolutePath} -o${outputPath.getAbsolutePath}".!
    if (exitCode != 0)
      throw new RuntimeException("Refinement Tree could not be visualized. This could be caused by the non-existence of the dot command.")
  }
}
