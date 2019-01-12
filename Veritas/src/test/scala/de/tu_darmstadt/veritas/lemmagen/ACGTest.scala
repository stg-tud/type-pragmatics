package de.tu_darmstadt.veritas.lemmagen

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{FunctionCallGraphGenerator, Problem}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.util.{AugmentedCallGraph, VeritasAugmentedCallGraph, VeritasAugmentedCallGraphBuilder}
import org.scalatest.FunSuite

import scala.sys.process.stringToProcess

class ACGTest extends FunSuite {
  val Directory = new File("augmented-call-graphs")
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val problem = new Problem(file)
  val dsk = problem.dsk

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  if (Directory.exists()) recursivedelete(Directory)
  Directory.mkdirs()

  def generateACG(function: FunctionDef): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] = {
    new VeritasAugmentedCallGraphBuilder(problem.spec).translate(function)(
      VeritasAugmentedCallGraph(function.signature.name)
    )
  }

  (dsk.staticFunctions ++ dsk.dynamicFunctions).foreach { fn =>
    test(fn.signature.name) {
      val acg = generateACG(fn)
      acg.visualizeACG(new File(Directory, s"acg-${fn.signature.name}.png"))
    }
  }

  test("make inter-function call graph") {
    val reduce = dsk.lookupByFunName(dsk.dynamicFunctions, "reduce").get
    val fct = new FunctionCallGraphGenerator(problem).generate(reduce)
    val dotFile = new File(Directory, "function-call-tree.dot")
    val writer = new BufferedWriter(new FileWriter(dotFile))
    writer.write(FunctionCallGraphGenerator.makeDotString(fct))
    writer.close()
    val outFile = new File(Directory, "function-call-tree.png")
    val exitCode = s"dot -Tpng ${dotFile.getAbsolutePath} -o${outFile.getAbsolutePath}".!
    if (exitCode != 0)
      throw new RuntimeException("Function Call Graph could not be visualized. This could be caused by the non-existence of the dot command.")
  }
}


