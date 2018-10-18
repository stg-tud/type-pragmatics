package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.LemmaGenSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

import scala.meta._

class SQLLemmaGenerationTest extends FunSuite {
  test("Read @Static and @Dynamic annotations from SQLSpec") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val module = new ScalaSPLTranslator().translate(file)
    val dsk = DomainSpecificKnowledgeBuilder().build(file)

    val enquirer = new LemmaGenSpecEnquirer(module, dsk)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))

    println(s"${dsk.progressProperties.size} progress properties:")
    println(dsk.progressProperties.map(_._2.name))
    println()
    println(s"${dsk.preservationProperties.size} preservation properties:")
    println(dsk.preservationProperties.map(_._2.name))
    println()

    def prettyPrintFunctions(funcs: Set[FunctionDef]): Unit = {
      outputPrettyPrinter.indent()
      for(fctn <- funcs) {
        fctn.signature.prettyPrint(outputPrettyPrinter)
        outputPrettyPrinter.newline()
      }
      outputPrettyPrinter.unindent()
    }
    println(s"${dsk.staticFunctions.size} static functions:")
    prettyPrintFunctions(dsk.staticFunctions)
    println()
    println(s"${dsk.dynamicFunctions.size} dynamic functions:")
    prettyPrintFunctions(dsk.dynamicFunctions)
    println()
    println(s"${dsk.predicates.size} predicates:")
    prettyPrintFunctions(dsk.predicates)
    println()
    println(s"${dsk.failableTypes.size} failable types:")
    println()
    for(ft <- dsk.failableTypes) {
      ft.prettyPrint(outputPrettyPrinter)
      outputPrettyPrinter.newline()
    }
    outputPrettyPrinter.flush()

    val lookupContextDef = dsk.staticFunctions.find(_.signature.name == "lookupContext").get

    for(argtype <- Seq("Select", "Table", "TType").map(SortRef(_))) {
      println(s"predicates involving $argtype:")
      println(enquirer.retrievePredicates(argtype).map(_.signature.name))
      println(s"producers of $argtype:")
      println(enquirer.retrieveProducers(argtype).map(_.signature.name))
      println(s"transformers of $argtype:")
      println(enquirer.retrieveTransformers(argtype).map(_.signature.name))
      println()
    }

    // print producers of failable types
    for(failable <- dsk.failableTypes) {
      val ref = SortRef(failable.name)
      println(s"producers of ${failable.name}:")
      println(enquirer.retrieveProducers(ref).map(_.signature.name))
    }

    println()

    // how to construct projectTableProgress:
    // select projectTable
    val projectTable = dsk.dynamicFunctions.find(_.signature.name == "projectTable").get
    // the return type is failable, so we construct the conclusion and assume it has some value
    val (noOptTable, someOptTable) = enquirer.retrieveFailableConstructors(projectTable.signature.out)
    // get the parameter types of projectTable
    val parameterTypes = projectTable.signature.in
    println(s"Parameter types of projectTable: ${parameterTypes}")
    // for each parameter type, ...
    for(pType <- parameterTypes) {
      // find predicates
      println(s"Predicates involving ${pType.name}: ${enquirer.retrievePredicates(pType).map(_.signature.name)}")
      println(s"Producers of ${pType.name}: ${enquirer.retrieveProducers(pType).map(_.signature.name)}")
    }
    // construct the conclusion of the typing rule
    val projectTableName = Term.Name(projectTable.signature.name)
    val projectTableArgs = projectTable.signature.in.map(ref => Term.fresh).toList
    val projectTableArgsTypes = projectTable.signature.in.map(ref => Type.Name(ref.name)).toList
    val projectTableParams = (projectTableArgs zip projectTableArgsTypes).map(
      { case (name: Term.Name, typ: Type.Name) => param"$name: $typ" })
    val resultName = Term.fresh
    val resultType = Type.Name(someOptTable.in(0).name)
    val constructorName = Term.Name(someOptTable.name)
    val conclusion =
      q"""exists (($resultName: $resultType) =>
         $projectTableName(..$projectTableArgs) == $constructorName($resultName))"""
    val rule = q"""def myRule(..$projectTableParams): Unit = { } ensuring $conclusion"""
    println(rule)
  }
}