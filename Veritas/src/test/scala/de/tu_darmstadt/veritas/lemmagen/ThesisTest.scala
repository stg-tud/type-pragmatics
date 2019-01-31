package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.PreservationGenerator
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
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec2.scala")
    val problem = new Problem(file)

    var swLemmas = problem.dsk.properties.map(tr => new Lemma(tr.name, tr.premises, tr.consequences))
    for(swLemma <- swLemmas) {
      val remaining = Oracle.pruneProvablyFalseLemmas(problem, Set(swLemma))
      val status = if(remaining contains swLemma) {
        "inconclusive"
      } else {
        "FALSE"
      }
      println(s"${swLemma.name}: $status")
    }
  }*/
/*
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

  test("preservation rawUnion") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val problem = new Problem(file)

    println("rawUnion")
    println("----------")

    val func = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "rawUnion").get
    val pred = problem.dsk.lookupByFunName(problem.dsk.staticFunctions, "welltypedRawtable").get
    val strat = new PreservationGenerator(problem, func, pred)//new ProgressStrategy(problem, func)
    val base = strat.generateBase()
    println(base)

    println("prune ...")
    val res = Oracle.pruneProvablyFalseLemmas(problem, Set(base))
    println(res)

    println("dropFirstColRaw")
    println("----------")
    val func2 = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "dropFirstColRaw").get
    val strat2 = new PreservationGenerator(problem, func2, pred)//new ProgressStrategy(problem, func)
    val base2 = strat2.generateBase()
    println(base2)

    println("prune ...")
    val res2 = Oracle.pruneProvablyFalseLemmas(problem, Set(base2))
    println(res2)

    /*println("projectTable")
    println("----------")
    val func3 = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "projectTable").get
    val pred2 = problem.dsk.lookupByFunName(problem.dsk.staticFunctions, "matchingAttrL").get
    val strat3 = new PreservationGenerator(problem, func3, pred2)//new ProgressStrategy(problem, func)
    val base3 = strat3.generateBase()
    println(base3)*/

    println("")/*
    val refinements = strat.expand(base.head)
    val refined = refinements.flatMap(_.refine(problem, base.head))
    printRules(refined)*/
  }*/

  test("reducers") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec2.scala")
    val problem = new Problem(file)
    for(fn <- problem.enquirer.retrieveReducers()) {
      println(s"${fn.signature.name}: ${problem.enquirer.getSideArgumentsTypes(fn)}")
      val sideArguments = problem.enquirer.getSideArgumentsTypes(fn)
      val staticFunctions = problem.enquirer.staticFunctions.filter(_.signature.in.intersect(sideArguments).nonEmpty)
      println(s"---> ${staticFunctions.map(_.signature.name)}")
    }
  }

  val Combinations = Seq(
   /* ("projectTable", "welltypedtable"),
    ("rawUnion", "welltypedRawtable"),*/
    ("projectCols", "welltypedRawtable")
  )
  for((funcName, predName) <- Combinations) {
    test(s"${funcName} / ${predName}") {
      val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec2.scala")
      val problem = new Problem(file)
      val func = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, funcName).get
      val pred = problem.dsk.lookupByFunName(problem.dsk.staticFunctions, predName).get
      val strat = new PreservationGenerator(problem, func, pred)
      val lemmas = strat.generate()
      printRules(lemmas)
      println("")
    }
  }
}
