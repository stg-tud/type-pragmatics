package de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, ProofGraph}

/**
  * Created by andiderp on 13/04/2017.
  */
class GraphVizVisualizer[Spec, Goal](override val graph: ProofGraph[Spec, Goal]) extends ProofGraphVisualizer[String, Spec, Goal] {
  val oblShape = "shape=box"
  val psShape = "shape=diamond"

  val oblPrefix = "obl"
  val psPrefix = "ps"

  private val builder: StringBuilder = StringBuilder.newBuilder

  override def getResult(): String = {
    val prefix = "digraph {\n"
    val suffix = "}"
    prefix + builder.toString + suffix
  }

  override def encodeObligation(obl: graph.Obligation): Unit = {
    val oblColor = if (fromObligation.contains(obl)) colorObl(obl) else "grey"
    val oblVisual = cleanLabel(labelObligation(obl))
    val style = Seq(oblShape, s"color=$oblColor", s"label=$oblVisual")//, "style=filled")
    encodeNode(oblPrefix + normalizeHashCode(obl.hashCode).toString, style)
  }

  private def labelObligation(obligation: graph.Obligation): String =
    obligations.collectFirst { case (name, `obligation`) =>
     name
    }.get

  private def cleanLabel(s: String): String = s.replaceAll("( |\\.|\\$|@|-)", "")

  private def colorObl(obl: graph.Obligation): String = {
    val ps = fromObligation(obl)
    val result = graph.verifiedBy(ps)
    val tuples = fromProofstep.get(ps) match {
      case Some(t) => t
      case None => sys.error(s"Could not find key in 'fromProofstep' map: $ps")
    }
    val goalVerified =
      result.nonEmpty &&
        result.get.status.isVerified &&
        ps.tactic.allRequiredOblsVerified(graph)(obl, tuples)
    if (goalVerified)
      "green"
    else
      "red"
  }

  private def encodeNode(referenceName: String, style: Seq[String]): Unit = {
    indent()
    builder.append(cleanLabel(referenceName))
    setAppearance(style)
    newline()
  }

  private def indent(): Unit = {
    val indentation = "    " // 4 spaces
    builder.append(indentation)
  }

  private def newline(): Unit = {
    builder.append(";\n")
  }

  private def setAppearance(options: Seq[String]): Unit = {
    if (options.nonEmpty) {
      builder.append(" [")
      val content = options.mkString(",")
      builder.append(content.substring(0, content.size))
      builder.append("]")
    }
  }

  def encodeProofStep(ps: graph.ProofStep): Unit = {
    val psColor = colorProofStep(ps)
    val psVisual = cleanLabel(labelProofStep(ps))
    val style = Seq(psShape, s"color=$psColor", s"label=$psVisual")//, "style=filled")
    encodeNode(psPrefix + normalizeHashCode(ps.hashCode).toString, style)
  }

  private def normalizeHashCode(hash: Int): Int = hash & Integer.MAX_VALUE


  private def labelProofStep(ps: graph.ProofStep): String = ps.tactic.getClass.getSimpleName

  private def colorProofStep(ps: graph.ProofStep): String = {
    val result = graph.verifiedBy(ps)
    if (result.isEmpty)
      "grey"
    else if (result.get.status.isVerified)
      "green"
    else
      "red"
  }

  override def linkToProofStep(obl: graph.Obligation, ps: graph.ProofStep): Unit = {
    indent()
    builder.append(oblPrefix + normalizeHashCode(obl.hashCode))
    builder.append(" -> ")
    builder.append(psPrefix + normalizeHashCode(ps.hashCode))
    newline()
  }

  override def linkFromProofStep(ps: graph.ProofStep, obl: graph.Obligation, edgeLabel: EdgeLabel): Unit = {
    indent()
    builder.append(psPrefix + normalizeHashCode(ps.hashCode))
    builder.append(" -> ")
    builder.append(oblPrefix + normalizeHashCode(obl.hashCode))
    val label = Seq(s"label=${cleanLabel(edgeLabel.desc)}")
    setAppearance(label)
    newline()
  }
}
