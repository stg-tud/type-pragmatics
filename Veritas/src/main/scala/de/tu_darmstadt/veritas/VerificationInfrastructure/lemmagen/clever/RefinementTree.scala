package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Refinement}

import scala.sys.process.stringToProcess

class RefinementTree[Tag](rootLemma: Lemma, rootTag: Tag) {
  class Node(val lemma: Lemma, val refinement: Option[Refinement], var tag: Tag) {
    private var _parent: Option[Node] = None
    private var _children: Seq[Node] = Seq()

    def addChild(child: Node): Unit = {
      require(child._parent.isEmpty)
      child._parent = Some(this)
      _children :+= child
    }

    def children: Seq[Node] = _children
    def parent: Option[Node] = _parent
    def descendants: Set[Node] = children.toSet ++ children.flatMap(_.descendants)
    def leaves: Set[Node] =
      if(children.isEmpty) {
        Set(this)
      } else {
        children.flatMap(_.leaves).toSet
      }
    def pathToRoot: Seq[Node] = parent match {
      case None => Seq(this)
      case Some(p) => this +: p.pathToRoot
    }

    def findRefinement(refinement: Refinement): Option[Node] = children.find(_.refinement.exists(r => r == refinement))
  }

  val root = new Node(rootLemma, None, rootTag)
  def nodes: Set[Node] = Set(root) ++ root.descendants
  def leaves: Set[Node] = root.leaves

  private def calculateNodeID(node: Node): String = {
    "n" + (node.hashCode() & Integer.MAX_VALUE)
  }

  def makeDotStringForNode(node: Node, sb: StringBuilder, nodeID: String): Unit = {
    sb.append(nodeID + " [shape=box, label\"\"];\n")
  }

  def makeDotString(): String = {
    val builder = StringBuilder.newBuilder
    for(node <- nodes) {
      makeDotStringForNode(node, builder, calculateNodeID(node))
    }
    builder.append("\n");
    for(node <- nodes; child <- node.children) {
      builder.append(calculateNodeID(node) + " -> " + calculateNodeID(child) + ";\n")
    }
    "digraph {\n" + builder.toString() + "}"
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
