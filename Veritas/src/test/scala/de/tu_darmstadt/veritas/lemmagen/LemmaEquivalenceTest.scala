package de.tu_darmstadt.veritas.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.LemmaEquivalence
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

class LemmaEquivalenceTest extends FunSuite {
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val module = new ScalaSPLTranslator().translate(file)
  val dsk = VeritasDomainSpecificKnowledgeBuilder().build(file)

  /*test("Reordering of typing rules") {
    val enquirer = new VeritasSpecEnquirer(module)
    val projectTableProgress = dsk.lookupByFunName(dsk.progressProperties, "projectTable")
  }*/

  (dsk.progressProperties.values.flatten ++ dsk.preservationProperties.values.flatten).foreach(rule => {
    test(s"Self-equivalence of ${rule.name}") {
      assert(LemmaEquivalence.isEquivalent(rule, rule))
    }
  })

}