package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}
import de.tu_darmstadt.veritas.backend.ast.{DataType, FunctionExpJudgment, Goals, TypingRule}

trait DomainSpecificKnowledgeBuilder[Specification <: SPLSpecification with SPLDomainSpecificKnowledgeAnnotations, Knowledge <: DomainSpecificKnowledge] {
  import scala.meta._

  def reporter: Reporter

  type MMap[K, V] = scala.collection.mutable.Map[K, V]

  var stats: Seq[Stat] = Seq()
  var adts: Map[Defn.Trait, Seq[Defn.Class]] = Map()

  def build(sourceFile: File): Knowledge = {
    val sourceString = scala.io.Source.fromFile(sourceFile).mkString("")
    build(sourceString)
  }

  // TODO we want that properties, equalities etcs (function defs)
  // are not defined in the language spec
  def build(sourceString: String): Knowledge = {
    val source = sourceString.parse[Source]
    val topLevelObject = ScalaMetaUtils.collectTopLevelObject(source.get)
    stats = topLevelObject.get.templ.stats
    adts = ADTCollector().collectADTs(stats)
    collectInformation()
    val base = buildBase()
    build(base)
  }

  protected var attachedProperties: MMap[(Defn.Def, String), Defn.Def] = scala.collection.mutable.Map()
  protected var recursiveFunctions: MMap[Defn.Def, Defn.Trait] = scala.collection.mutable.Map()
  protected var propertyNeeded: MMap[Defn.Def, (String, Seq[Case])] = scala.collection.mutable.Map()
  protected var distinction: MMap[Case, Seq[Defn.Def]] = scala.collection.mutable.Map()

  // fill maps or data types
  protected def collectInformation(): Unit = {
    stats.foreach {
      case fn: Defn.Def =>
        if(ScalaMetaUtils.containsAnnotation(fn.mods, "Distinction"))
          collectDistinction(fn)
        if(ScalaMetaUtils.containsAnnotation(fn.mods, "PropertyAttached")) {
          val annots = ScalaMetaUtils.collectAnnotations(fn.mods)
          val filteredAnnots = annots.filter { _.init.tpe.toString == "PropertyAttached" }
          filteredAnnots.foreach { annot =>
            collectProperty(fn, annot)
          }
        }
        if(ScalaMetaUtils.containsAnnotation(fn.mods, "PropertyNeeded")) {
          val annots = ScalaMetaUtils.collectAnnotations(fn.mods)
          val filteredAnnots = annots.filter { _.init.tpe.toString == "PropertyNeeded" }
          filteredAnnots.foreach { annot =>
            collectPropertyNeeded(fn, annot)
          }
        }
        if(ScalaMetaUtils.containsAnnotation(fn.mods, "Recursive"))
          collectRecursive(fn)
      case _ => ()
    }
  }

  private def collectDistinction(fn: Defn.Def): Unit = {
    val annot = collectAnnotation(fn.mods, "Distinction")
    val functionEqPosition = annot.init.argss.head.head.asInstanceOf[Lit.Int].value
    val foundCase = collectCaseAtPosition(fn, functionEqPosition)
    val distinctionCriteriaNames = annot.init.argss.head.tail.map { _.asInstanceOf[Lit.String].value }
    val distinctionCriteriaDefs = distinctionCriteriaNames.flatMap { collectFunctionDef(_, "DistinctionCriteria") }
    distinction += foundCase -> distinctionCriteriaDefs
  }

  def collectAnnotation(mods: Seq[Mod], name: String): Mod.Annot = {
    mods.collect { case annot: Mod.Annot if annot.init.tpe.toString == name => annot}.head
  }

  private def collectCaseAtPosition(fn: Defn.Def, index: Int): Case = {
    fn.body match {
      case Term.Match(_, cases) => cases(index)
      case _ => reporter.report("Top level construct of function has to be a match", fn.body.pos.startLine)
    }
  }

  private def collectFunctionDef(name: String, annotName: String): Option[Defn.Def] =
    stats.collect {
      case fn: Defn.Def
        if ScalaMetaUtils.containsAnnotation(fn.mods, annotName) && fn.name.value == name => fn
    }.headOption

  private def collectProperty(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val propertyReference = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    collectFunctionDef(propertyReference, "Property") match {
      case Some(propertyDef) =>
        attachedProperties += (fn, propertyDef.name.value) -> propertyDef
      case None => reporter.report("Property reference could not be found", annot.pos.startLine)
    }
  }

  private def collectPropertyNeeded(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val propertyName = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    val propertyDef = collectFunctionDef(propertyName, "Property").head
    val functionEqPositions = annot.init.argss.tail.map { _.asInstanceOf[Lit.Int].value }
    val functionEqs = functionEqPositions.map { pos => collectCaseAtPosition(fn, pos) }
    propertyNeeded += propertyDef -> (fn.name.value, functionEqs)
  }

  private def collectRecursive(fn: Defn.Def): Unit = {
    val annot = collectAnnotation(fn.mods, "Recursive")
    val positions = annot.init.argss.head.map { _.asInstanceOf[Lit.Int].value }
    // we know that at least one possible has to be given
    if (positions.isEmpty)
      reporter.report("At least one index has to be given for the Recursive annotation.", annot.pos.startLine)
    val outer = fn.paramss.head(positions.head)
    val adtTrait =
      if (positions.tail.nonEmpty)
        collectInnerADT(outer, positions.tail)
      else
        getTraitForLastParam(outer)

    recursiveFunctions += fn -> adtTrait
  }

  private def getTraitForLastParam(param: Term.Param): Defn.Trait = {
    val paramTrait = adts.filterKeys { key =>
      key.name.value == param.decltpe.get.toString
    }.headOption
    paramTrait.get._1
  }

  private def collectInnerADT(param: Term.Param, nextPositions: Seq[Int]): Defn.Trait = {
    if (nextPositions.isEmpty)
      getTraitForLastParam(param)
    else {
      val paramTrait = getTraitForParam(param)
      val ctor = adts(paramTrait).head
      val params = ctor.ctor.paramss.head
      val selectedParam = params(nextPositions.head)
      collectInnerADT(selectedParam, nextPositions.tail)
    }
  }

  private def getTraitForParam(param: Term.Param): Defn.Trait = {
    val paramTrait = adts.filterKeys { key =>
      key.name.value == param.decltpe.get.toString
    }.headOption
    if (paramTrait.get._2.size != 1)
      reporter.report("The ADT of an argument of a recursive marked function should only have one constructor.", param.pos.startLine)
    paramTrait.get._1
  }

  def buildBase(): DomainSpecificKnowledge = {
    val transAttachedProps = translateAttachedProperties()
    val transNeededProps = translatePropertiesNeeded()
    val transRecursiveFuncs = translateRecursiveFunctions()
    new DomainSpecificKnowledge {
      override val attachedProperties = transAttachedProps
      override val propertiesNeeded = transNeededProps
      override val recursiveFunctions = transRecursiveFuncs
    }
  }

  def translateAttachedProperties(): Map[(FunctionDef, String), TypingRule] = {
    val functionTranslator = SPLFunctionDefinitionTranslator(reporter, adts)
    val ensuringFunctionTranslator = SPLEnsuringFunctionTranslator(reporter)

    attachedProperties.map { case ((fdef, name), typing) =>
      (functionTranslator.translateFunction(fdef), name) -> ensuringFunctionTranslator.translateEnsuringFunction(typing)
    }.toMap
  }

  def translatePropertiesNeeded(): Map[TypingRule, Seq[FunctionEq]] = {
    val functionTranslator = SPLFunctionDefinitionTranslator(reporter, adts)
    val ensuringFunctionTranslator = SPLEnsuringFunctionTranslator(reporter)
    propertyNeeded.map { case (typing, (fnname, cases)) =>
      val transTyping = ensuringFunctionTranslator.translateEnsuringFunction(typing)
      val transCases = cases.map { c => functionTranslator.translateCase(fnname, c) }
      transTyping -> transCases
    }.toMap
  }

  def translateRecursiveFunctions(): Map[FunctionDef, DataType] = {
    val functionTranslator = SPLFunctionDefinitionTranslator(reporter, adts)
    val adtTranslator = SPLAlgebraicDataTypeTranslator(reporter)
    recursiveFunctions.map { case (fn, tr) =>
        val cases = adts(tr)
        functionTranslator.translateFunction(fn) -> adtTranslator.translateADT(tr, cases)
    }.toMap
  }

  def build(base: DomainSpecificKnowledge): Knowledge
}

object DomainSpecificKnowledgeBuilder {
  def apply(): DomainSpecificKnowledgeBuilder[SPLSpecification, DomainSpecificKnowledge] =
    new DomainSpecificKnowledgeBuilder[SPLSpecification, DomainSpecificKnowledge] {
      override val reporter = Reporter()
      override def build(base: DomainSpecificKnowledge): DomainSpecificKnowledge = base
    }
}
