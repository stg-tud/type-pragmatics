package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.meta._

trait SPLFunctionDefinitionTranslator {
  def reporter: Reporter
  def adts: Map[Defn.Trait, Seq[Defn.Class]]

  def translateFunction(fn: Defn.Def): FunctionDef = {
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
    FunctionSig(name.value, sortRefs, SortRef(returnType.toString()))
  }

  private def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      reporter.report("The parameter definition has no type defined ")
    val sortRefs = params.map { param =>
      SortRef(param.decltpe.get.toString)
    }
    sortRefs
  }

  def translateCase(funName: String, cas: Case): FunctionEq = {
    val patterns = translateCasePattern(cas.pat)
    val funTranslator = FunctionTranslator(Seq())
    val body = funTranslator.translateExp(cas.body)
    FunctionEq(funName, patterns, body)
  }

  private def translateCasePattern(pat: Pat): Seq[FunctionPattern] = {
    pat match {
      case Pat.Extract(term, pattern) =>
        if (!onlySelfDefinedCotrsUsed(term.toString))
          reporter.report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        val args = pattern.map { translateInnerCasePattern }
        Seq(FunctionPatApp(term.toString, args))
      case Pat.Tuple(pattern) => pattern.map { translateInnerCasePattern }
      case Pat.Var(name) => Seq(FunctionPatVar(name.value))
      case _ => reporter.report("Other pattern than tuple, application or variable is used", pat.pos.startLine)
    }
  }

  private def onlySelfDefinedCotrsUsed(name: String): Boolean = {
    adts.exists { case (_, cotrs) =>
      cotrs.exists { _.name.value == name}
    }
  }

  private def translateInnerCasePattern(pat: Pat): FunctionPattern = {
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        if (!onlySelfDefinedCotrsUsed(term.toString))
          reporter.report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        FunctionPatApp(term.toString, args)
      case Pat.Var(name) => FunctionPatVar(name.value)
      case _ => reporter.report("Other pattern than application or variable is used", pat.pos.startLine)
    }
  }
}

object SPLFunctionDefinitionTranslator {
  def apply(r: Reporter, a: Map[Defn.Trait, Seq[Defn.Class]]): SPLFunctionDefinitionTranslator = {
    new SPLFunctionDefinitionTranslator {
      override val reporter: Reporter = r
      override val adts: Map[Defn.Trait, Seq[Defn.Class]] = a
    }
  }
}
