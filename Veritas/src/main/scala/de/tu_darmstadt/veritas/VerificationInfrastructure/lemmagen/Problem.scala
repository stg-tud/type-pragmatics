package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

class Problem(specFile: File) {
  val spec = new ScalaSPLTranslator().translate(specFile)
  val dsk = DomainSpecificKnowledgeBuilder().build(specFile)
  val enquirer = new LemmaGenSpecEnquirer(spec, dsk)

  def preservationFunctions: Set[FunctionDef] = {
    enquirer.dynamicFunctions
  }

  def progressFunctions: Set[FunctionDef] = {
    enquirer.dynamicFunctions.filter(fn => enquirer.isFailableType(fn.signature.out))
  }
}