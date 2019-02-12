package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints.AdditionalPremise
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{PreservationGenerator, ProgressGenerator}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.{PreservationStrategy, ProgressStrategy}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExp}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.translator.FunctionExpressionTranslator
import org.scalatest.FunSuite

import scala.meta.Term
import scala.meta.inputs.Input

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

  test("parse hint") {
    val hint = "_ == ttcons(_, _, ttempty())"
    //val hint = "welltypedtable(tt, t)"
    val input = Input.VirtualFile("annotation/hint.scala", hint)
    val tree = input.parse[scala.meta.Term].get

    var currentIndex = 1
    var vars = Seq[String]()
    def nextFreshName(): String = {
      val name = s"_$currentIndex"
      vars +:= name
      currentIndex += 1
      name
    }
    val transformed = tree.transform {
      case Term.Placeholder() => Term.Name(nextFreshName())
    }.asInstanceOf[Term]
    println(vars)
    val translator = new FunctionExpressionTranslator(vars)
    val exp = translator.translateExp(transformed)
    constructPremise(vars.map(name => MetaVar(name)), exp)
  }

  def constructPremise(vars: Seq[MetaVar], hint: FunctionExp): VeritasConstruct = {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec2.scala")
    val problem = new Problem(file)
    val judgment = FunctionExpJudgment(hint)
    val varTypes = problem.enquirer.getAllVarTypes(judgment)
    val constraints = vars.map(mv => Constraint.fresh(varTypes(mv)))
    val assignment = Assignments.generate(constraints).head
    val renaming = vars.zip(assignment).toMap
    val renamed = LemmaEquivalence.VariableRenamer(judgment, renaming)
    println(renamed)
    judgment
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


  test("lemma oracle") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec2.scala")
    val problem = new Problem(file)

    var swLemmas = problem.dsk.properties.map(tr => new Lemma(tr.name, tr.premises, tr.consequences))
    for(swLemma <- swLemmas.toSeq.sortBy(_.name)) {
      try {
        val remaining = Oracle.pruneProvablyFalseLemmas(problem, Set(swLemma))
        val status = if (remaining contains swLemma) {
          "inconclusive"
        } else {
          "FALSE"
        }
        println(s"${swLemma.name}: $status")
      } catch {
        case x: Exception => println(s"${swLemma.name}: $x")
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
/*
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
    /*("projectTable", "welltypedtable"),
    ("rawUnion", "welltypedRawtable"),
    ("filterTable", "welltypedtable"),
    ("filterRows", "welltypedRawtable"),*/
    //("dropFirstColRaw", "welltypedRawtable"),
    //("projectCols", "welltypedRawtable"),
    ("attachColToFrontRaw", "welltypedRawtable")
  )
  for((funcName, predName) <- Combinations) {
    test(s"${funcName} / ${predName}") {
      val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
      val problem = new Problem(file)
      val func = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, funcName).get
      val hints = problem.dsk.additionalPremises.getOrElse(func, Seq.empty).map(new AdditionalPremise(problem, _))
      val pred = problem.dsk.lookupByFunName(problem.dsk.staticFunctions, predName).get
      val strat = new PreservationGenerator(problem, func, pred, hints)
      val lemmas = strat.generate()
      println(s"===== ${lemmas.size} lemmas!")
      printRules(lemmas)
      println("")

      val expectedLemmas = problem.dsk.preservationProperties(func)
      for(expected <- expectedLemmas) {
        val equivalentLemmas = lemmas.filter(entry => LemmaEquivalence.isEquivalent(expected, entry))
        println(s"Equivalent to ${expected.name}: ${equivalentLemmas.length} out of ${lemmas.length}")
      }
    }
  }
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val problem = new Problem(file)
  for(func <- problem.dsk.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.signature.out))) {
    test(s"progress ${func.signature.name}") {
      val strat = new ProgressGenerator(problem, func, Seq())
      println("additional:" + problem.dsk.additionalPremises.getOrElse(func, Seq.empty))
      val lemmas = strat.generate()
      println(s"===== ${lemmas.size} lemmas!")
      printRules(lemmas)
      println("")

      val expectedLemmas = problem.dsk.progressProperties.getOrElse(func, Set())
      for (expected <- expectedLemmas) {
        val equivalentLemmas = lemmas.filter(entry => LemmaEquivalence.isEquivalent(expected, entry))
        println(s"Equivalent to ${expected.name}: ${equivalentLemmas.length} out of ${lemmas.length}")
      }
    }
  }
}
