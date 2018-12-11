package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import de.tu_darmstadt.veritas.scalaspl.util.{VeritasAugmentedCallGraph, VeritasAugmentedCallGraphBuilder}

/**
  * top-level strategy for automatically generating a proof graph for progress/preservation proofs
  * assumes a ScalaSPL specification which is properly annotated and contains progress/preservation top-level theorem and all necessary auxiliary lemmas
  * @param pathtoScalaSPLsource
  * @param pathToStore
  */
case class ProgressPreservationTopLevelStrategy(pathtoScalaSPLsource: String, pathToStore: String) extends InitializationStrategy[VeritasConstruct, VeritasFormula] {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val sourcefile = new File(pathtoScalaSPLsource)
  val SPLSpec: Module = new ScalaSPLTranslator().translate(sourcefile)

  //collect domain-specific knowledge
  val builder = DomainSpecificKnowledgeBuilder()
  val dsk = builder.build(sourcefile)



  //initialize proof graph store
  val storefile = new File(pathToStore)
  recursivedelete(storefile) //simply overwrite any old folder
  //try to create new folder
  if (!storefile.mkdir()) sys.error("Could not create new proof graph store at " + pathToStore)

  //create a new empty graph
  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](storefile) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]

  //handle for simpler access to proof graph parts
  val PG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](g, ProofGraphUI.extractGoalOrLemmaName)

  // register all types in graph once to Xodus library
  PropertyTypes.registerWrapperType(g.store)

  //convenience function
  def generateGraph(): ProofGraph[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] = {
    initializePG(SPLSpec)
  }

  override def initializePG(s: VeritasConstruct): ProofGraph[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] = {
    //look for top-level reduce function by name
    val reducefun: FunctionDef = dsk.lookupByFunName(dsk.dynamicFunctions, "reduce") match {
      case Some(fd) => fd
      case _ => sys.error("Could not locate a top-level reduce function (with name `reduce`)")
    }

    //construct augmented call graph from reduce function on
    val acg = new VeritasAugmentedCallGraphBuilder(SPLSpec).translate(reducefun)(VeritasAugmentedCallGraph())

    //add root progress/preservation obligations to graph
    val progress_tr: TypingRule = dsk.lookupByFunName(dsk.progressProperties, "reduce").head
    val progressObligation: g.Obligation = g.newObligation(SPLSpec, Goals(Seq(progress_tr), None))
    g.storeObligation("Progress", progressObligation)

    val preservation_tr: TypingRule = dsk.lookupByFunName(dsk.preservationProperties, "reduce").head
    val preservationObligation: g.Obligation = g.newObligation(SPLSpec, Goals(Seq(preservation_tr), None))
    g.storeObligation("Preservation", preservationObligation)

    g
  }

  //visualize proof graph
  def visualizeGraph(filename: String) {
    val graphfile = new File(filename)
    Dot(g, graphfile)
  }


}
