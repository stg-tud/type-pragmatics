package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpApp}
import de.tu_darmstadt.veritas.newinputdsl.translator.FunctionExpressionTranslator

case class RegisteredTermFunctionExpressionTranslator() extends FunctionExpressionTranslator(Seq()) {
  import scala.meta._
  override def translateExp(term: Term): FunctionExp = term match {
    case Lit.String(string) if ReflectionHelper.termRegistered(string) =>
      FunctionExpApp(s"ODTRef_$string", Seq())
    case _ => super.translateExp(term)
  }
}
