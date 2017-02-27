package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}


/**
  * Created by sylvia on 27/02/2017.
  */
class DDG_SQLExamples {
  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._

  val queryroot = ExpressionDomainNode[VeritasConstruct, VeritasConstruct](~'query, Map())

}
