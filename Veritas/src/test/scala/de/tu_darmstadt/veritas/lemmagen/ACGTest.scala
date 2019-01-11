package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.util.{AugmentedCallGraph, VeritasAugmentedCallGraph, VeritasAugmentedCallGraphBuilder}
import org.scalatest.FunSuite

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

  if(Directory.exists()) recursivedelete(Directory)
  Directory.mkdirs()

  def generateACG(function: FunctionDef): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta]  = {
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
}


