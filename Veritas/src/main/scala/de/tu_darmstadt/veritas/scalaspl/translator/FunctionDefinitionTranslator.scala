package de.tu_darmstadt.veritas.scalaspl.translator

import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.scalaspl.util.Reporter

import scala.meta._

trait FunctionDefinitionTranslator {
  def reporter: Reporter
  def adts: Map[Defn.Trait, Seq[Defn.Class]]

  def translate(fn: Defn.Def): FunctionDef = {
    // decltype has to be given
    if (fn.decltpe.isEmpty)
      reporter.report(s"The return type of function ${fn.name.value} has to be explicitly defined", fn.pos.startLine)
    if (fn.tparams.nonEmpty)
      reporter.report(s"A function definition does not allow type parameters (${fn.name.value})", fn.pos.startLine)
    val signature = translateFunctionSignature(fn.name, fn.paramss.head, fn.decltpe.get)
    val equations = fn.body match {
      // TODO: check that the expr over which is matched is a tuple in the correct order of function params
      case Term.Match(_, cases) =>
        cases.map { translateCase(fn.name.value, _) }
        // TODO is there a better way for functions without a function definition?
      case Term.Name("???") => Seq()
      case _ =>
        reporter.report(s"Top level construct of function ${fn.name.value} has to be a match", fn.body.pos.startLine)
    }
    FunctionDef(signature, equations)
  }

  private def translateFunctionSignature(name: Term.Name, params: Seq[Term.Param], returnType: Type): FunctionSig = {
    val sortRefs = correctParamList(params)
    FunctionSig(name.value, sortRefs, translateType(returnType))
  }

  private def translateType(typ: Type): SortRef = {
    if (onlySelfDefinedCotrsUsed(typ.toString))
      reporter.report("The type was not defined in the specification and is not Boolean", typ.pos.startLine)
    else if (typ.toString == "Boolean")
      SortRef("Bool")
    else
      SortRef(typ.toString)
  }

  private def onlySelfDefinedTypesUsed(name: String): Boolean = {
    val isADT = adts.exists { case (typ, _) => typ.name.value == name }
    isADT || name == "Boolean"
  }

  private def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      reporter.report("The parameter definition has no type defined ")
    val sortRefs = params.map { param =>
      translateType(param.decltpe.get)
    }
    sortRefs
  }

  def translateCase(funName: String, cas: Case): FunctionEq = {
    val patterns = translateCasePattern(cas.pat)
    val funTranslator = new FunctionExpressionTranslator(Seq())
    val body = funTranslator.translateExp(cas.body)
    FunctionEq(funName, patterns, body)
  }

  private def translateCasePattern(pat: Pat): Seq[FunctionPattern] = {
    implicit val wildcardNames: FreshNames = new FreshNames()
    pat match {
      case Pat.Extract(term, pattern) =>
        if (!onlySelfDefinedCotrsUsed(term.toString))
          reporter.report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        val args = pattern.map { translateInnerCasePattern }
        Seq(FunctionPatApp(term.toString, args))
      case Pat.Tuple(pattern) => pattern.map { translateInnerCasePattern }
      case Pat.Var(name) => Seq(FunctionPatVar(name.value))
      case Pat.Wildcard() => Seq(FunctionPatVar(wildcardNames.freshName(FunctionDefinitionTranslator.reservedVarNameInPattern)))
      case _ => reporter.report("Other pattern than tuple, application or variable is used", pat.pos.startLine)
    }
  }

  private def onlySelfDefinedCotrsUsed(name: String): Boolean = {
    adts.exists { case (_, cotrs) =>
      cotrs.exists { _.name.value == name}
    }
  }

  private def translateInnerCasePattern(pat: Pat)(implicit wildcardNames: FreshNames): FunctionPattern = {
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        if (!onlySelfDefinedCotrsUsed(term.toString))
          reporter.report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        FunctionPatApp(term.toString, args)
      case Pat.Var(name) if name.value != FunctionDefinitionTranslator.reservedVarNameInPattern =>
        FunctionPatVar(name.value)
      case Pat.Var(name) =>
        reporter.report(
          s"The reserved name ${FunctionDefinitionTranslator.reservedVarNameInPattern}[0-9]+ was used within pattern",
          pat.pos.startLine)
      case Pat.Wildcard() =>
        FunctionPatVar(wildcardNames.freshName(FunctionDefinitionTranslator.reservedVarNameInPattern))
      case _ => reporter.report("Other pattern than application or variable is used", pat.pos.startLine)
    }
  }
}

object FunctionDefinitionTranslator {
  // TODO anything better?
  val reservedVarNameInPattern = "wildcardName"
  def apply(r: Reporter, a: Map[Defn.Trait, Seq[Defn.Class]]): FunctionDefinitionTranslator = {
    new FunctionDefinitionTranslator {
      override val reporter: Reporter = r
      override val adts: Map[Defn.Trait, Seq[Defn.Class]] = a
    }
  }
}
