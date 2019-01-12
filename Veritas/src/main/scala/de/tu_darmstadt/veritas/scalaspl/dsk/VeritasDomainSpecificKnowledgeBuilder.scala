package de.tu_darmstadt.veritas.scalaspl.dsk

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}
import de.tu_darmstadt.veritas.backend.ast.{DataType, TypingRule}
import de.tu_darmstadt.veritas.scalaspl.lang.{DomainSpecificKnowledgeAnnotations, ScalaSPLSpecification}
import de.tu_darmstadt.veritas.scalaspl.translator.{AlgebraicDataTypeTranslator, EnsuringFunctionTranslator, FunctionDefinitionTranslator}
import de.tu_darmstadt.veritas.scalaspl.util.{AlgebraicDataTypeCollector, Reporter, ScalaMetaUtils}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait VeritasDomainSpecificKnowledgeBuilder[Specification <: ScalaSPLSpecification with DomainSpecificKnowledgeAnnotations, Knowledge <: VeritasDomainSpecificKnowledge] {
  import scala.meta._

  def reporter: Reporter

  var stats: Seq[Stat] = Seq()
  var adts: Map[Defn.Trait, Seq[Defn.Class]] = Map()

  def build(sourceFile: File): Knowledge = {
    val sourceString = scala.io.Source.fromFile(sourceFile).mkString("")
    build(sourceString)
  }

  def build(sourceString: String): Knowledge = {
    val source = sourceString.parse[Source]
    val topLevelObject = ScalaMetaUtils.collectTopLevelObject(source.get)
    stats = topLevelObject.get.templ.stats
    adts = AlgebraicDataTypeCollector().collectADTs(stats)
    stats.foreach { stat =>
      collectInformation(stat)
    }
    val base = buildBase()
    build(base)
  }

  protected val recursiveFunctions: mutable.Map[Defn.Def, Defn.Trait] = mutable.Map()
  protected val failableTypes: ListBuffer[Defn.Trait] = ListBuffer()
  protected val progressProperties: mutable.MutableList[(Defn.Def, Defn.Def)] = new mutable.MutableList[(Defn.Def, Defn.Def)]()
  protected val preservationProperties: mutable.MutableList[(Defn.Def, Defn.Def)] = new mutable.MutableList[(Defn.Def, Defn.Def)]()
  protected val staticFunctions: mutable.Set[Defn.Def] = mutable.Set()
  protected val dynamicFunctions: mutable.Set[Defn.Def] = mutable.Set()
  protected val properties: mutable.Set[Defn.Def] = mutable.Set()

  protected def withSuper[S](construct: S)(supcollect: S => Unit)(f: PartialFunction[S, Unit]): Unit = {
    if (f.isDefinedAt(construct))
      f(construct)
    supcollect(construct)
  }

  // collect the information needed to build the domain specific knowledge trait
  protected def collectInformation(stat: Stat): Unit = stat match {
    case fn: Defn.Def =>
      if(ScalaMetaUtils.containsAnnotation(fn.mods, "Recursive"))
        collectRecursive(fn)
      if(ScalaMetaUtils.containsAnnotation(fn.mods, "Dynamic"))
        dynamicFunctions += fn
      if(ScalaMetaUtils.containsAnnotation(fn.mods, "Static"))
        staticFunctions += fn
      if(ScalaMetaUtils.containsAnnotation(fn.mods, "Property"))
        properties += fn
      progressProperties ++= collectLinkingAnnotation(fn, "ProgressProperty")(collect)
      preservationProperties ++= collectLinkingAnnotation(fn, "PreservationProperty")(collect)
    case tr: Defn.Trait =>
      if (ScalaMetaUtils.containsAnnotation(tr.mods, "FailableType"))
        failableTypes += tr
    case _ => ()
  }

  private def collect(fn: Defn.Def, prop: Defn.Def, annot: Mod.Annot): (Defn.Def, Defn.Def) = fn -> prop

  protected def collectLinkingAnnotation[K, V](fn: Defn.Def, annotName: String)(mapper: (Defn.Def, Defn.Def, Mod.Annot) => (K, V)): Seq[(K, V)] = {
    if(ScalaMetaUtils.containsAnnotation(fn.mods, annotName)) {
      val annots = ScalaMetaUtils.collectAnnotations(fn.mods)
      val filteredAnnots = annots.filter { _.init.tpe.toString == annotName }
      filteredAnnots.map { annot =>
        collectPropertyLinked(fn, annot, mapper)
      }
    } else Seq()
  }

  protected def collectAnnotation(mods: Seq[Mod], name: String): Mod.Annot = {
    mods.collect { case annot: Mod.Annot if annot.init.tpe.toString == name => annot}.head
  }

  protected def collectPropertyLinked[K, V](fn: Defn.Def, annot: Mod.Annot, mapper: (Defn.Def, Defn.Def, Mod.Annot) => (K, V)): (K, V) = {
    val propertyReference = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    collectFunctionDef(propertyReference, "Property") match {
      case Some(propertyDef) =>
        mapper(fn, propertyDef, annot)
      case None => reporter.report("Property reference could not be found", annot.pos.startLine)
    }
  }

  private def collectPropertyNeeded(fn: Defn.Def, prop: Defn.Def, annot: Mod.Annot): (Defn.Def, (String, Seq[Case])) = {
    val positions = annot.init.argss.head.tail
    if (positions.isEmpty)
      reporter.report(s"PropertyNeeded of function ${fn.name.value} should have at least one position for case state", annot.pos.startLine)
    val functionEqPositions = positions.map { _.asInstanceOf[Lit.Int].value }
    val functionEqs = functionEqPositions.map { pos => collectCaseAtPosition(fn, pos) }
    prop -> (fn.name.value, functionEqs)
  }

  private def collectPropertyAttached(fn: Defn.Def, prop: Defn.Def, annot: Mod.Annot): ((Defn.Def, String), Defn.Def) =
    (fn, prop.name.value) -> prop


  protected def collectFunctionDef(name: String, annotName: String): Option[Defn.Def] =
    stats.collectFirst {
      case fn: Defn.Def
        if ScalaMetaUtils.containsAnnotation(fn.mods, annotName) && fn.name.value == name => fn
    }

  protected def collectCaseAtPosition(fn: Defn.Def, index: Int): Case = {
    fn.body match {
      case Term.Match(_, cases) => cases(index)
      case _ => reporter.report("Top level construct of function has to be a match", fn.body.pos.startLine)
    }
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

  def buildBase(): VeritasDomainSpecificKnowledge = {
    val transRecursiveFuncs = translateRecursiveFunctions()
    val transFailableTypes = translateTrait(failableTypes)
    val transProgressProps = translateProperties(progressProperties)
    val transPreservationProps = translateProperties(preservationProperties)
    val transStaticFuncs = translateFunctions(staticFunctions.toSet)
    val transDynamicFuncs = translateFunctions(dynamicFunctions.toSet)
    // TODO: This can be removed once we are sure we have linked all properties
    val transProperties = translateProperty(properties.toSet)
    new VeritasDomainSpecificKnowledge {
      override val recursiveFunctions: Map[FunctionDef, DataType] = transRecursiveFuncs
      override val failableTypes: Seq[DataType] = transFailableTypes
      override val preservationProperties: Map[FunctionDef, Set[TypingRule]] = transPreservationProps
      override val progressProperties: Map[FunctionDef, Set[TypingRule]] = transProgressProps
      override val staticFunctions: Set[FunctionDef] = transStaticFuncs
      override val dynamicFunctions: Set[FunctionDef] = transDynamicFuncs
      override val properties: Set[TypingRule] = transProperties
    }
  }

  private def translateRecursiveFunctions(): Map[FunctionDef, DataType] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    recursiveFunctions.map { case (fn, tr) =>
        val cases = adts(tr)
        functionTranslator.translate(fn) -> adtTranslator.translate(tr, cases)
    }.toMap
  }

  private def translateFunctions(definitions: Set[Defn.Def]): Set[FunctionDef] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    definitions.map(functionTranslator.translate)
  }

  private def translateProperty(property: Set[Defn.Def]): Set[TypingRule] = {
    val propertyTranslator = EnsuringFunctionTranslator(reporter)
    property.map(propertyTranslator.translate)
  }

  private def translateProperties(properties: Seq[(Defn.Def, Defn.Def)]): Map[FunctionDef, Set[TypingRule]] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val propertyTranslator = EnsuringFunctionTranslator(reporter)
    val propertyMap = new mutable.HashMap[FunctionDef, mutable.Set[TypingRule]]
      with mutable.MultiMap[FunctionDef, TypingRule]
    properties.foreach {
      case (fn, prop) => propertyMap.addBinding(functionTranslator.translate(fn), propertyTranslator.translate(prop))
    }
    propertyMap.mapValues(_.toSet).toMap
  }

  protected def translateTrait(traits: Seq[Defn.Trait]): Seq[DataType] = {
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    traits.map { tr =>
      val cases = adts(tr)
      adtTranslator.translate(tr, cases)
    }
  }

  def build(base: VeritasDomainSpecificKnowledge): Knowledge
}

object VeritasDomainSpecificKnowledgeBuilder {
  def apply(): VeritasDomainSpecificKnowledgeBuilder[ScalaSPLSpecification, VeritasDomainSpecificKnowledge] =
    new VeritasDomainSpecificKnowledgeBuilder[ScalaSPLSpecification, VeritasDomainSpecificKnowledge] {
      override val reporter = Reporter()
      override def build(base: VeritasDomainSpecificKnowledge): VeritasDomainSpecificKnowledge = base
    }
}
