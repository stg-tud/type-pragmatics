package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

/** A Problem encapsulates a ScalaSPL specification, its respective
  * domain-specific knowledge and a `LemmaGenSpecEnquirer`.
  */
class Problem(val specFile: File) {
  val spec = new ScalaSPLTranslator().translate(specFile)
  val dsk = VeritasDomainSpecificKnowledgeBuilder().build(specFile)
  val enquirer = new LemmaGenSpecEnquirer(spec, dsk)
}