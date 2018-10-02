package de.tu_darmstadt.veritas.scalaspl.translator

import java.io.File

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.scalaspl.util.{AlgebraicDataTypeCollector, Reporter, ScalaMetaUtils}

import scala.collection.mutable.ListBuffer
import scala.meta._

class ScalaSPLTranslator(dskAnnotationsToExclude: Seq[String] = Seq("Property")) {
  val reporter = Reporter()

  var adts: Map[Defn.Trait, Seq[Defn.Class]] = Map()

  def translate(sourceFile: File): Module = {
    val sourceString = scala.io.Source.fromFile(sourceFile).mkString("")
    translate(sourceString)
  }

  def translate(sourceString: String): Module = {
    val parsedSource = sourceString.parse[Source]
    parsedSource.toEither match {
      case Left(error) => throw error.details
      case Right(source) =>
        collectTopLevelObject(source) match {
          case Some(o) =>
            translateObject(o)
          case None => reporter.report("The top level construct is not an object")
        }
    }
  }

  private def collectTopLevelObject(source: Source): Option[Defn.Object] = source.collect {
    case o: Defn.Object => o
  }.headOption

  private def translateObject(o: Defn.Object): Module = {
    // check if it extends ScalaSPLSpecification
    if (o.templ.inits.nonEmpty && o.templ.inits.head.tpe.toString == "ScalaSPLSpecification") {
      val moduleName = o.name.value
      val locals = collectLocalBlocks(o.templ.stats)
      val transLocals = locals.map { translateLocal }
      val defs = translateStats(o.templ.stats)
      Module(moduleName, Seq(), defs ++ transLocals)
    } else reporter.report(s"Object ${o.name.value} does not inherit from the trait ScalaSPLSpecification")
  }

  private def translateStats(stats: Seq[Stat]): Seq[ModuleDef] = {
    adts = AlgebraicDataTypeCollector().collectADTs(stats)
    val ensuringFunctionTranslator = EnsuringFunctionTranslator(reporter)
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    val translatedDataTypes = adts.map { case (base, cases) => adtTranslator.translate(base, cases) }
    val axioms = collectAxioms(stats)
    val lemmas = collectLemmas(stats)
    val goals = collectGoals(stats)
    val partialFunctions = collectPartialFunctions(stats).diff(axioms ++ lemmas ++ goals)
    val functions = collectFunctions(stats).diff(axioms ++ lemmas ++ goals ++ partialFunctions)
    val translatedPartialFunctions = partialFunctions.map { d => PartialFunctions(Seq(functionTranslator.translate(d))) }
    val translatedFunctions = functions.map { d => Functions(Seq(functionTranslator.translate(d))) }
    val translatedAxioms = axioms.map { ensuringFunctionTranslator.translate }
    val translatedLemmas = lemmas.map { ensuringFunctionTranslator.translate }
    val translatedGoals = goals.map { ensuringFunctionTranslator.translate }
    translatedDataTypes.toSeq ++
      translatedPartialFunctions ++
      translatedFunctions ++
      Seq(Axioms(translatedAxioms)) ++
      Seq(Lemmas(translatedLemmas, None)) ++
      Seq(Goals(translatedGoals, None))
  }

  private def collectPartialFunctions(stats: Seq[Stat]): Seq[Defn.Def] =
    stats.collect {
      // has no goal, axiom, lemma annotation
      case fn: Defn.Def
        // want to ignore properties and criterias that are attached to functions for domain specific knowledge
        if fn.name.value != "typable" && ScalaMetaUtils.notContainsOneAnnotation(fn.mods, dskAnnotationsToExclude) &&
          ScalaMetaUtils.containsAnnotation(fn.mods, "Partial") =>
        fn
    }

  private def collectFunctions(parsed: Seq[Stat]): Seq[Defn.Def] =
    parsed.collect {
      // has no goal, axiom, lemma annotation
      case fn: Defn.Def
        // want to ignore properties and criterias that are attached to functions for domain specific knowledge
        if fn.name.value != "typable" &&
          ScalaMetaUtils.notContainsOneAnnotation(fn.mods, dskAnnotationsToExclude) =>
          fn
    }

  private def collectAxioms(parsed: Seq[Stat]): Seq[Defn.Def] = collectFunctionAnnotatedWith(parsed, "Axiom")
  private def collectLemmas(parsed: Seq[Stat]): Seq[Defn.Def] = collectFunctionAnnotatedWith(parsed, "Lemma")
  private def collectGoals(parsed: Seq[Stat]): Seq[Defn.Def] = collectFunctionAnnotatedWith(parsed, "Goal")

  private def collectFunctionAnnotatedWith(parsed: Seq[Stat], annotation: String): Seq[Defn.Def] = parsed.collect {
    case fn: Defn.Def => fn
  }.filter { fn =>
    val annots = fn.mods.collect{ case anot: Mod.Annot if anot.init.tpe.toString == annotation => anot}
    annots.nonEmpty
  }

  private def collectLocalBlocks(parsed: Seq[Stat]): Seq[Defn.Trait] = parsed.collect {
    case tr: Defn.Trait if ScalaMetaUtils.containsAnnotation(tr.mods, "Local") => tr
  }

  private def translateLocal(block: Defn.Trait): Local = {
    // TODO: what do we need to check for?
    if(block.templ.inits.nonEmpty)
      reporter.report("A local block can not extend another trait", block.pos.startLine)
    val valBlocks = collectValBlocks(block.templ.stats)
    val translatedValBlocks = valBlocks.map { case (vals, different) => translateConstantBlock(vals, different) }
    Local(translateStats(block.templ.stats) ++ translatedValBlocks)
  }

  private def collectValBlocks(stats: Seq[Stat]): Seq[(Seq[Decl.Val], Boolean)] = {
    val result = ListBuffer[(ListBuffer[Decl.Val], Boolean)]()
    stats.foreach {
      case v: Defn.Val => reporter.report("Defintion of vals within a local block are not allowed", v.pos.startLine)
      case v: Decl.Val if ScalaMetaUtils.containsAnnotation(v.mods, "Different") =>
        result += ListBuffer[Decl.Val](v) -> true
      case v: Decl.Val =>
        if (result.isEmpty)
          result += ListBuffer[Decl.Val](v) -> false
        else
          result.last._1 += v
      case _ =>
        result += ListBuffer[Decl.Val]() -> false
    }
    result.toList.map { block => (block._1.toList, block._2) }
  }

  private def translateConstantBlock(vals: Seq[Decl.Val], different: Boolean): Consts =
    Consts(vals.map { translateConstant }, different)

  private def translateConstant(v: Decl.Val): ConstDecl = {
    if (v.pats.size > 1)
      reporter.report("Multiple variable declaration is not supported", v.pos.startLine)
    val constName = v.pats.head match {
      case Pat.Var(name) => name.value
      case _ => reporter.report("Another pattern than simple variable assignment was used", v.pats.head.pos.startLine)
    }
    ConstDecl(constName, SortRef(v.decltpe.toString()))
  }
}

object ScalaSPLTranslator {
  val predefTraits = Seq("Expression", "Context", "Typ")

  val predefTypes = Seq("Bool", "iType")
}
