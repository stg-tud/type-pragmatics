package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.{BufferedWriter, File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Oracle, Problem, Refinement}

import scala.sys.process.stringToProcess

sealed trait NodeStatus
case class Unknown() extends NodeStatus
case class Inconclusive() extends NodeStatus
case class ProvablyFalse() extends NodeStatus

class RefinementNode(val lemma: Lemma, val refinement: Option[Refinement]) {
  private var _parent: Option[RefinementNode] = None
  private var _children: Seq[RefinementNode] = Seq()
  private var _status: NodeStatus = Unknown()

  def addChild(child: RefinementNode): Unit = {
    require(child._parent.isEmpty)
    child._parent = Some(this)
    _children :+= child
  }

  def status: NodeStatus = _status
  def children: Seq[RefinementNode] = _children
  def parent: Option[RefinementNode] = _parent
  def descendants: Set[RefinementNode] = children.toSet ++ children.flatMap(_.descendants)
  def leaves: Set[RefinementNode] =
    if(children.isEmpty) {
      Set(this)
    } else {
      children.flatMap(_.leaves).toSet
    }
  def pathToRoot: Seq[RefinementNode] = parent match {
    case None => Seq(this)
    case Some(p) => this +: p.pathToRoot
  }

  def refine(problem: Problem, refinement: Refinement): RefinementNode = {
    findRefinement(refinement) match {
      case Some(child) => child
      case None =>
        refinement.refine(problem, lemma) match {
          case Some(refinedLemma) =>
            val child = new RefinementNode(refinedLemma, Some(refinement))
            addChild(child)
            child
          case None => this
        }
    }
  }

  def setStatusRecursively(status: NodeStatus): Unit = {
    _status = status
    if(status == ProvablyFalse()) {
      parent match {
        case Some(node) => node.setStatusRecursively(status)
        case _ =>
      }
    }
  }

  def findRefinement(refinement: Refinement): Option[RefinementNode] = children.find(_.refinement.exists(r => r == refinement))

  def makeDotString(sb: StringBuilder, nodeID: String): Unit = {
    val color = status match {
      case Unknown() => "gray"
      case ProvablyFalse() => "red"
      case Inconclusive() => "black"
    }
    val label = "\"" + lemma.refinements.last.toString + "\""
    sb.append(nodeID + s" [shape=box, label=$label, color=$color];\n")
  }
}

class RefinementTree(rootLemma: Lemma) {
  val root = new RefinementNode(rootLemma, None)
  def nodes: Set[RefinementNode] = Set(root) ++ root.descendants
  def leaves: Set[RefinementNode] = root.leaves

  private def calculateNodeID(node: RefinementNode): String = {
    "n" + (node.hashCode() & Integer.MAX_VALUE)
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

  def prune(problem: Problem, nodes: Seq[RefinementNode]): Unit = {
    val unknownLemmas = nodes.collect {
      case node if node.status == Unknown() => node.lemma
    }.toSet
    val remaining = Oracle.pruneProvablyFalseLemmas(problem, unknownLemmas)
    for (node <- nodes) {
      if (remaining contains node.lemma) {
        println(s"set to INCONCLUSIVE: ${node.lemma}")
        node.setStatusRecursively(Inconclusive())
      } else {
        println(s"set to FALSE: ${node.lemma}")
        node.setStatusRecursively(ProvablyFalse())
      }
    }
  }
}
