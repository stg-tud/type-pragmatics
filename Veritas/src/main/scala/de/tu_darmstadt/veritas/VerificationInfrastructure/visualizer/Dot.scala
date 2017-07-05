package de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphXodus}
import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct

import scala.sys.process.stringToProcess

/**
  * The command dot has to be available to use these methods
  */
object Dot {
  /**
    * Generates a visualization with the dot command with the help of the GrahVizVisualizer of a proofgraph.
    * @param pg The proofgraph which is visualized.
    * @param outputPath File which contains visualization. It has to end with .png.
    */
  def apply[Spec, Goal](pg: ProofGraph[Spec, Goal], outputPath: File): Unit = {
    // TODO make it possible to use different visualizer?
    val viz = new GraphVizVisualizer(pg)
    val dotFormatted = viz.visualize()
    val dotFile = new File(outputPath.getParentFile, outputPath.getName.replace(".png", ".dot"))
    dotFile.createNewFile()
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(dotFormatted)
    writer.close()
    // dot -T<fileformat> <pathtodotfile> -o<outputpath>
    val exitCode = s"dot -Tpng ${dotFile.getAbsolutePath} -o${outputPath.getAbsolutePath}".!
    if (exitCode != 0)
      throw new RuntimeException("Graph could not be visualized. This could be caused by the non-existance of the dot command.")
  }
}
