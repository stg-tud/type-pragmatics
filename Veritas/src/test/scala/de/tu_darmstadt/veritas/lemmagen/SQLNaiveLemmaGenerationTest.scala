package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, FileWriter, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import org.scalatest.FunSuite

class SQLNaiveLemmaGenerationTest extends FunSuite {
  val MaxPremises = 4
  val ExcludeProperties = Seq(
    // incompatible schemas:
    "welltypedEmptyProjection",
    "projectFirstRawPreservesWelltypedRaw",
    "findColPreservesWelltypedRaw",
    "attachColToFrontRawPreservesWellTypedRaw",
    "dropFirstColRawPreservesWelltypedRaw",
    "attachColToFrontRawPreservesRowCount"
  )

  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
  val lemmaPrettyPrinter = new SimpleToScalaSPLSpecificationPrinter {
    override val printer: PrettyPrintWriter = outputPrettyPrinter
  }
  val problem = new Problem(file)
  val dsk = problem.dsk

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val generatedDirectory = new File("generated-lemmas")
  if (generatedDirectory.exists()) recursivedelete(generatedDirectory)
  generatedDirectory.mkdirs()

  def writeLemmasToFile(lemmas: Seq[(Lemma, Int)], file: File): Unit = {
    println(s"Writing ${lemmas.length} lemmas to ${file} ...")
    val writer = new FileWriter(file)
    val lemmaWriter = new SimpleToScalaSPLSpecificationPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    for ((lemma, idx) <- lemmas) {
      writer.write(s"BEGIN Lemma #$idx:\n")
      writer.write(s"${lemma.premises.length} premises\n")
      writer.write("--------------\n")
      lemmaWriter.printTypingRule(lemma)
      writer.write("\n")
      writer.write("Refinements:\n")
      for (refinement <- lemma.refinements) {
        writer.write("  " + refinement + "\n")
      }
      writer.write("END\n")
      writer.flush()
    }
  }

  def testLemmaGenerator(lemmaKind: String, name: String, lemmas: Seq[Lemma], expected: TypingRule): Unit = {
    val generatedLemmas: Seq[(Lemma, Int)] = lemmas.zipWithIndex
    val equivalentLemmas = generatedLemmas.filter(entry => LemmaEquivalence.isEquivalent(expected, entry._1))
    println(s"Equivalent to ${expected.name}: ${equivalentLemmas.length} out of ${generatedLemmas.length}")
    println()
    for ((lemma, idx) <- equivalentLemmas) {
      lemmaPrettyPrinter.printTypingRule(lemma)
      outputPrettyPrinter.flush()
      println()
    }
    writeLemmasToFile(generatedLemmas, new File(generatedDirectory, s"$lemmaKind-$name-generated.txt"))
    if (equivalentLemmas.nonEmpty)
      writeLemmasToFile(equivalentLemmas.toSeq, new File(generatedDirectory, s"$lemmaKind-$name-equivalent-${expected.name}.txt"))
    assert(equivalentLemmas.nonEmpty)
  }

  test("All @Property-annotated lemmas belong to a function") {
    val Ignore = Set("reduceProgress", "reducePreservation")
    val propertiesWithAnnotation = dsk.progressProperties.toSeq ++ dsk.preservationProperties.toSeq
    val propertiesWithoutFunctions = dsk.properties.filter {
      prop =>
        val functions = propertiesWithAnnotation.collect {
          case (fn, fnProperties) if fnProperties.contains(prop) => fn
        }
        functions.isEmpty
    }.map(_.name).diff(Ignore)
    assert(propertiesWithoutFunctions.isEmpty, s"${propertiesWithoutFunctions} have no function")

  }

  def testProperties(kind: String,
                     properties: Map[FunctionDef, Set[TypingRule]])
                    (builder: (Problem, FunctionDef) => RefinementStrategy): Unit = {
    properties.foreach {
      case (function, expectedLemmas) =>
        val strategy = builder(problem, function)
        val generator = new LimitedDepthLemmaGenerator(problem, strategy, MaxPremises)
        lazy val lemmas = {
          generator.generate()
        }
        expectedLemmas.foreach(expected => {
          test(s"$kind ${expected.name}") {
            if (ExcludeProperties contains expected.name)
              cancel("excluded in ExcludeProperties")

            testLemmaGenerator(kind, function.signature.name, lemmas, expected)
          }
        })
    }
  }

  testProperties("progress", dsk.progressProperties)((problem, fn) => new naive.ProgressStrategy(problem, fn))
  testProperties("preservation", dsk.preservationProperties)((problem, fn) => new naive.PreservationStrategy(problem, fn))
}