package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.MetaVar

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

class RefinementNode(val tree: RefinementTree,
                     val lemma: Lemma,
                     val refinement: Option[Refinement],
                     val preVariables: Set[MetaVar],
                     val postVariables: Set[MetaVar]) {
  private var _parents: Seq[RefinementNode] = Seq()
  private var _children: Map[Refinement, RefinementNode] = Map()
  var oracleStatus: OracleStatus = Unknown()
  var direct: Boolean = false
  var refinementStatus: RefinementStatus = ShouldRefine()
  var selected: Boolean = false

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

  def refine(problem: Problem, refinement: Refinement): RefinementNode = {
    val (pre, post) = PrePostVariables.calculatePrePostVariables(
      preVariables, postVariables, refinement
    )
    refinement.refine(problem, lemma) match {
      case None => this
      case Some(refinedLemma) =>
        tree.findLemma(refinedLemma) match {
          case Some(node) => {
            require(node.preVariables == pre)
            require(node.postVariables == post)
            this.addChild(node, refinement)
            node
          }
          case None =>
            val child = new RefinementNode(tree, refinedLemma, Some(refinement), pre, post)
            this.addChild(child, refinement)
            child
        }
    }
  }

  def findRefinement(refinement: Refinement): Option[RefinementNode] = children.find(_.refinement.exists(r => r == refinement))

  def setStatusRecursively(status: OracleStatus, isDirect: Boolean = true): Unit = {
    this.oracleStatus = status
    this.direct = isDirect
    for(parent <- parents)
      parent.setStatusRecursively(status, false)
  }

  def makeDotString(sb: StringBuilder, nodeID: String): Unit = {
    val color = oracleStatus match {
      case Unknown() => "gray"
      case Incorrect() if direct => "red"
      case Incorrect() => "magenta"
      case Inconclusive() if selected => "green"
      case Inconclusive() => "white"
    }

    val label = ("\"" + lemma.toString.replace("\n", "\\n")
                + s"\\n$oracleStatus\n$refinementStatus"
                + s"\\npre=$preVariables\npost=$postVariables" + "\"")
    sb.append(nodeID + s" [shape=box, label=$label, fillcolor=$color, style=filled];\n")
  }
}

class RefinementTree(rootLemma: Lemma) {
  val root = new RefinementNode(this, rootLemma, None, rootLemma.boundVariables, Set())
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
    for(node <- nodes; (refinement, child) <- node.refinedChildren) {
      builder.append(s"${calculateNodeID(node)} -> ${calculateNodeID(child)} [label=" + "\"" + refinement + "\"];\n")
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
