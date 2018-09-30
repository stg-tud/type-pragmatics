package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Solve, StructuralInduction}
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator


class AESoundnessProofGraph(storefile: File) {

  val sourcefile = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala")
  val fullAESpec: Module = new ScalaSPLTranslator().translate(sourcefile)

  //collect domain-specific knowledge
  val builder = DomainSpecificKnowledgeBuilder()
  val dsk = builder.build(sourcefile)

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](storefile) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]

  // register all types in graph once to Xodus library
  PropertyTypes.registerWrapperType(g.store)

  val specenq = new VeritasSpecEnquirer(fullAESpec)

  //handle for simpler access to proof graph parts
  val PG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](g, ProofGraphUI.extractGoalOrLemmaName)

  def addSolveTacticToAllLeaves(): Unit = {
    val leaves = g.leaves()
    for (o <- leaves) {
      g.applyTactic(o, Solve[VeritasConstruct, VeritasFormula])
    }
  }

  //obtain translated progress root obligation from built domain-specific knowledge
  val progress_tr: TypingRule = dsk.lookupByFunName(dsk.progressProperties, "reduce").head
  val progressObligation: g.Obligation = g.newObligation(fullAESpec, Goals(Seq(progress_tr), None))
  g.storeObligation("Progress", progressObligation)

  //apply top-level induction
  private val rootInduction = StructuralInduction(MetaVar("t1"), fullAESpec, specenq)
  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInduction)
  addSolveTacticToAllLeaves()

}

object ConstructAESoundnessGraph extends App {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val file = new File("AESoundnessProofGraph-store")
  recursivedelete(file) //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for AESoundnessProofGraph-store.")

  val pg = new AESoundnessProofGraph(file)
  val PG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](pg.g, ProofGraphUI.extractGoalOrLemmaName)

  //print progress obligation in ScalaSPL (using ScalaSPL pretty printer)
  val prettyPrinter = new SimpleToScalaSPLSpecificationPrinter {
    override val printer: PrettyPrintWriter = new PrettyPrintWriter(new PrintWriter(System.out))
  }
  prettyPrinter.printTypingRule(pg.progress_tr)
  //since we call pretty printing only for a typing rule and not for an entire model, we need to close the writer
  // manually in order to see the remaining parts of the typing rule printed
  prettyPrinter.printer.close()

  //visualize proof graph
  def visualizeGraph(filename: String) {
    val graphfile = new File(filename)
    if (file.exists()) recursivedelete(file)
    Dot(pg.g, graphfile)
  }

  visualizeGraph("ae_soundness.png")

}
