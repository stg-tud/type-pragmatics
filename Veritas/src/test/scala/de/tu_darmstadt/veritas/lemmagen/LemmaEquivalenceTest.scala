package de.tu_darmstadt.veritas.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.LemmaEquivalence
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

/** Sanity check: check that every baseline lemma is \approx-equivalent to itself */
class LemmaEquivalenceTest extends FunSuite {
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val module = new ScalaSPLTranslator().translate(file)
  val dsk = VeritasDomainSpecificKnowledgeBuilder().build(file)

  (dsk.progressProperties.values.flatten ++ dsk.preservationProperties.values.flatten).foreach(rule => {
    test(s"Self-equivalence of ${rule.name}") {
      assert(LemmaEquivalence.isEquivalent(rule, rule))
    }
  })

}