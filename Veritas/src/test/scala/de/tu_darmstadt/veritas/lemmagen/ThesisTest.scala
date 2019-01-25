package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.{PreservationStrategy, ProgressStrategy}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import org.scalatest.FunSuite

class ThesisTest extends FunSuite {
  test("write lemma") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }
    problem.dsk.properties.foreach { prop =>
      problem.enquirer.getAllVarTypes(prop) // needed so that quantified variables have the right type
      lemmaPrettyPrinter.printTypingRule(prop)
    }
    outputPrettyPrinter.flush()
  }

  def printRules(lemmas: Seq[Lemma]) = {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }
    lemmas.foreach { lemma =>
      lemmaPrettyPrinter.printTypingRule(lemma)
    }
    outputPrettyPrinter.flush()
  }

  /*
  test("lemma oracle") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    var swLemmas = problem.dsk.properties.filter(_.name.startsWith("somewhatWrong"))
    var run = true

    swLemmas = Oracle.pruneProvablyFalseLemmas(problem, swLemmas)
    println(s"remaining: ${swLemmas.map(_.name)}")
  }*/

  test("static functions") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val problem = new Problem(file)
    println(problem.dsk.staticFunctions.map(f => s"\\C{${f.signature.name}}").mkString(", "))
  }

  test("dynamic functions") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val problem = new Problem(file)
    println(problem.dsk.dynamicFunctions.map(f => s"\\C{${f.signature.name}}").mkString(", "))
  }

  test("progress strategy") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }

    val projectTable = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "projectTable").get
    val strat = new ProgressStrategy(problem, projectTable)

    val base = strat.generateBase()
    strat.generateBase().foreach{ l =>
      lemmaPrettyPrinter.printTypingRule(l)
      println()
    }
    outputPrettyPrinter.flush()
  }

  def formatFunctionName(f: FunctionDef): String = {
    s"\\C{${f.signature.name}}"
  }

  test("auxiliary functions") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    def sorts(s: Set[String]): Set[SortRef] = s.map(l => SortRef(l))

    println(problem.enquirer.retrievePredicates(sorts(Set("Table", "RawTable"))).map(_.signature.name))
    println(problem.enquirer.retrieveTransformers(sorts(Set("Table"))).map(_.signature.name))
    println(problem.enquirer.retrieveProducers(sorts(Set("Table"))).map(_.signature.name))

  }

  test("pres strat") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }

    val projectCols = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "projectCols").get
    val strat = new PreservationStrategy(problem, projectCols)

    val base = strat.generateBase()
    val first = base.head
    strat.expand(first).flatMap(_.refine(problem, first)).foreach { l =>
      lemmaPrettyPrinter.printTypingRule(l)
      println()
    }
    outputPrettyPrinter.flush()
  }

  test("progress projectTable") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val problem = new Problem(file)

    val func = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "projectCols").get
    val strat = new PreservationStrategy(problem, func)//new ProgressStrategy(problem, func)
    val base = strat.generateBase()
    printRules(base)

    println("")/*
    val refinements = strat.expand(base.head)
    val refined = refinements.flatMap(_.refine(problem, base.head))
    printRules(refined)*/
  }
}
