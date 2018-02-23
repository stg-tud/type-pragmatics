package de.tu_darmstadt.veritas.newinputdsl.translator

import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.newinputdsl.util.Reporter

import scala.meta._

trait DistinctionCriteriaTranslator {
  def reporter: Reporter

  def translate(fn: Defn.Def): FunctionExpJudgment = {
    // decltype has to be given
    fn.decltpe match {
      case None => reporter.report(s"The return type of function ${fn.name.value} has to be explicitly defined", fn.pos.startLine)
      case Some(Type.Name("Boolean")) => ()
      case Some(_) => reporter.report(s"The return type of function ${fn.name.value} has to be Boolean", fn.pos.startLine)
    }
    if (fn.tparams.nonEmpty)
      reporter.report(s"A function definition does not allow type parameters (${fn.name.value})", fn.pos.startLine)
    val metaBindings = fn.paramss.headOption.getOrElse(Nil).map { _.name.value }
    val functionTranslator = FunctionTranslator(metaBindings)
    val functionBody = functionTranslator.translateExp(fn.body)
    FunctionExpJudgment(functionBody)
  }
}

object DistinctionCriteriaTranslator {
  def apply(r: Reporter): DistinctionCriteriaTranslator = {
    new DistinctionCriteriaTranslator {
      override val reporter: Reporter = r
    }
  }
}
