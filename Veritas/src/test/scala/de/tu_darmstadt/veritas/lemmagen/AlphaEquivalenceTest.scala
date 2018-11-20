package de.tu_darmstadt.veritas.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

class AlphaEquivalenceTest extends FunSuite {
  test("Reordering of typing rules") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val module = new ScalaSPLTranslator().translate(file)
    val dsk = DomainSpecificKnowledgeBuilder().build(file)

    val enquirer = new VeritasSpecEnquirer(module)
    val projectTableProgress = dsk.lookupByFunName(dsk.progressProperties, "projectTable")
  }
}