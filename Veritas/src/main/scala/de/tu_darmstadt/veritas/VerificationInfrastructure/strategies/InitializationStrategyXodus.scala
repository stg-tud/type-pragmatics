package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot

trait InitializationStrategyXodus[Spec, Goal] extends InitializationStrategy[Spec, Goal]{

  val spec: Spec
  val pathToStore: String
  val goalname_extractor: Goal => String

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete)
    file.delete
  }

  //initialize proof graph store
  val storefile = new File(pathToStore)

  recursivedelete(storefile) //simply overwrite any old folder
  //try to create new folder
  if (!storefile.mkdir()) sys.error("Could not create new proof graph store at " + pathToStore)

  //create a new empty graph
  override val g: ProofGraphXodus[Spec, Goal] with ProofGraphTraversals[Spec, Goal] =
    new ProofGraphXodus[Spec, Goal](storefile) with ProofGraphTraversals[Spec, Goal]

  //handle for simpler access to proof graph parts
  val PG_UI = new ProofGraphUI[Spec, Goal](g, goalname_extractor)

  // register all types in graph once to Xodus library
  PropertyTypes.registerWrapperType(g.store)

  override def initializePG(gs: Set[Goal]): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    for (goal <- gs) {
      val newobl = g.newObligation(spec, goal, goalname_extractor(goal))
      g.storeObligation(goalname_extractor(goal), newobl)
    }
    g
  }

  //visualize proof graph
  def visualizeGraph(filename: String) {
    val graphfile = new File(filename)
    Dot(g, graphfile)
  }
}
