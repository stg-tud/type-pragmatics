package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.ProgressStrategy
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

  test("lemma oracle") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    problem.dsk.properties.foreach { lemma =>
      if(lemma.name.startsWith("somewhatWrong")) {
        val s = Oracle.invoke(problem, lemma) match {
          case Oracle.Inconclusive() => "inconclusive"
          case Oracle.ProvablyTrue() => "lemma is provably TRUE"
          case Oracle.ProvablyFalse() => "lemma is provably FALSE"
        }
        println(lemma.name, s)
      }
    }
  }

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
}
