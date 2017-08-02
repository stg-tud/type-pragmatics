package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.{Module, VeritasConstruct}

import scala.util.{Failure, Success}

/**
  * Verifier for translating Veritas AST to TPTP (with selected transformation strategy) and calling
  * Vampire on the result
  */
class TPTPVampireVerifier(timeout: Int = 10, version: String = "4.1", logictype: String = "tff") extends TPTPVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPVampireVerifier"

  override def prover: Prover[TPTP] = VampireTPTP(timeout = timeout, version = version)

  override def transformer: VeritasTransformer[TPTP] =
    if (logictype == "tff")
      new VeritasTransformer[TPTP](
        Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
          Simplification -> Simplification.LogicalAndConstructors,
          VariableEncoding -> VariableEncoding.InlineEverything,
          Selection -> Selection.SelectAll,
          Problem -> Problem.All)), x => x.asInstanceOf[TPTP])
    else
      new VeritasTransformer[TPTP](
        Configuration(Map(FinalEncoding -> FinalEncoding.BareFOF,
          Simplification -> Simplification.LogicalAndConstructors,
          VariableEncoding -> VariableEncoding.InlineEverything,
          Selection -> Selection.SelectAll,
          Problem -> Problem.All)), x => x.asInstanceOf[TPTP])


}
