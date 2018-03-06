package de.tu_darmstadt.veritas.newinputdsl.dsk

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}
import de.tu_darmstadt.veritas.backend.ast.{DataType, FunctionExpJudgment, TypingRule}
import de.tu_darmstadt.veritas.newinputdsl.lang.{SPLDomainSpecificKnowledgeAnnotations, SPLSpecification}
import de.tu_darmstadt.veritas.newinputdsl.translator.{AlgebraicDataTypeTranslator, DistinctionCriteriaTranslator, EnsuringFunctionTranslator, FunctionDefinitionTranslator}
import de.tu_darmstadt.veritas.newinputdsl.util.{ADTCollector, Reporter, ScalaMetaUtils}

import scala.collection.mutable.ListBuffer

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

  protected val attachedProperties: MMap[(Defn.Def, String), Defn.Def] = scala.collection.mutable.Map()
  protected val recursiveFunctions: MMap[Defn.Def, Defn.Trait] = scala.collection.mutable.Map()
  protected val propertyNeeded: MMap[Defn.Def, (String, Seq[Case])] = scala.collection.mutable.Map()
  protected val distinction: MMap[(String, Case), Seq[Defn.Def]] = scala.collection.mutable.Map()
  protected val groupings: ListBuffer[(String, Seq[Case])] = ListBuffer()
  protected val typesOfMetaVars: MMap[(Defn.Def, String), Defn.Trait] = scala.collection.mutable.Map()

  protected val expressions: ListBuffer[Defn.Trait] = ListBuffer()
  protected val contexts: ListBuffer[Defn.Trait] = ListBuffer()
  protected val types: ListBuffer[Defn.Trait] = ListBuffer()

  def collectCategoryOfDataType(tr: Defn.Trait): Unit = {
    val supertraits = tr.templ.inits.map { _.tpe.toString }
    if (supertraits.contains("Expression"))
      expressions += tr
    if (supertraits.contains("Context"))
      contexts += tr
    if (supertraits.contains("Typ"))
      types += tr
  }

  // fill maps or data types
  protected def collectInformation(): Unit = {
    stats.foreach {
      case fn: Defn.Def =>
        collectSpecificAnnotation(fn, "Distinction", collectDistinction)
        collectSpecificAnnotation(fn, "PropertyAttached", collectPropertyAttached)
        collectSpecificAnnotation(fn, "PropertyNeeded", collectPropertyNeeded)
        collectSpecificAnnotation(fn, "GroupedDistinction", collectGroupedDistinction)
        if(ScalaMetaUtils.containsAnnotation(fn.mods, "Recursive"))
          collectRecursive(fn)
        if (ScalaMetaUtils.containsOneOfAnnotations(fn.mods, Seq("Lemma", "Axiom", "Goal")))
          collectMetaVarType(fn)
      case tr: Defn.Trait =>
        collectCategoryOfDataType(tr)
      case _ => ()
    }
  }

  def collectGroupedDistinction(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val functionEqGroupings: Seq[Seq[Int]] = collectSequence(annot.init.argss.head)
    val functionEqs = functionEqGroupings.map { seq =>
      seq.map { pos =>
        collectCaseAtPosition(fn, pos)
      }
    }
    groupings ++= functionEqs.map { (fn.name.value, _) }
  }

  def collectSequence(list: List[Term]): Seq[Seq[Int]] = {
    if(list.isEmpty)
      reporter.report("GroupedDistinction has to have at least one sequence.")

    list.map {
      case Term.Apply(Term.Name("Seq"), args) =>
        args.map {
          _.asInstanceOf[Lit.Int].value
        }
      case elem => reporter.report("GroupedDistinction needs to get only Seq[Int] passed.", elem.pos.startLine)
    }
  }

  protected def collectSpecificAnnotation(fn: Defn.Def, annotName: String, annotationCollector: (Defn.Def, Mod.Annot) => Unit): Unit = {
    if(ScalaMetaUtils.containsAnnotation(fn.mods, annotName)) {
      val annots = ScalaMetaUtils.collectAnnotations(fn.mods)
      val filteredAnnots = annots.filter { _.init.tpe.toString == annotName }
      filteredAnnots.foreach { annot =>
        annotationCollector(fn, annot)
      }
    }
  }

  private def collectDistinction(fn: Defn.Def): Unit = {
    val annot = collectAnnotation(fn.mods, "Distinction")
    val functionEqPosition = annot.init.argss.head.head.asInstanceOf[Lit.Int].value
    val foundCase = collectCaseAtPosition(fn, functionEqPosition)
    val distinctionCriteriaNames = annot.init.argss.head.tail.map { _.asInstanceOf[Lit.String].value }
    val distinctionCriteriaDefs = distinctionCriteriaNames.flatMap { collectFunctionDef(_, "DistinctionCriteria") }
    distinction += (fn.name.value, foundCase) -> distinctionCriteriaDefs
  }

  private def collectDistinction(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val criteriaName = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    val functionEqPosition = annot.init.argss.head.tail.head.asInstanceOf[Lit.Int].value
    val foundCase = collectCaseAtPosition(fn, functionEqPosition)
    val distinctionCriteriaDef = collectFunctionDef(criteriaName, "DistinctionCriteria")
    if (distinctionCriteriaDef.isEmpty)
      reporter.report(s"The referenced distinction criteria ${criteriaName} was not found.", annot.pos.startLine)
    val prevCriterias = distinction.getOrElse((fn.name.value, foundCase), Seq())
    distinction += (fn.name.value, foundCase) -> (prevCriterias.seq :+ distinctionCriteriaDef.get)
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

  private def collectPropertyAttached(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val propertyReference = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    collectFunctionDef(propertyReference, "Property") match {
      case Some(propertyDef) =>
        attachedProperties += (fn, propertyDef.name.value) -> propertyDef
      case None => reporter.report("Property reference could not be found", annot.pos.startLine)
    }
  }

  private def collectPropertyNeeded(fn: Defn.Def, annot: Mod.Annot): Unit = {
    val propertyName = annot.init.argss.head.head.asInstanceOf[Lit.String].value
    val propertyDef = collectFunctionDef(propertyName, "Property")
    if (propertyDef.isEmpty)
      reporter.report(s"Property ${propertyName} could not be found", annot.pos.startLine)
    val positions = annot.init.argss.head.tail
    if (positions.isEmpty)
      reporter.report(s"PropertyNeeded of function ${fn.name.value} should have at least one position for case state", annot.pos.startLine)
    val functionEqPositions = positions.map { _.asInstanceOf[Lit.Int].value }
    val functionEqs = functionEqPositions.map { pos => collectCaseAtPosition(fn, pos) }
    propertyNeeded += propertyDef.get -> (fn.name.value, functionEqs)
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

  def collectMetaVarType(fn: Defn.Def): Unit =
    fn.paramss.head.map { param =>
      typesOfMetaVars += (fn, param.name.value) -> getTraitForLastParam(param)
    }

  def buildBase(): DomainSpecificKnowledge = {
    val transAttachedProps = translateAttachedProperties()
    val transNeededProps = translatePropertiesNeeded()
    val transRecursiveFuncs = translateRecursiveFunctions()
    val transGroupings = translateGrouping()
    val transDistinctions = translateDistinctions()
    val transMetaVarTypes = translateTypesOfMetaVars()
    val transExprs = translateCategory(expressions)
    val transCtxs = translateCategory(contexts)
    val transTypes = translateCategory(types)
    new DomainSpecificKnowledge {
      override val attachedProperties: Map[(FunctionDef, String), TypingRule] = transAttachedProps
      override val propertiesNeeded: Map[TypingRule, Seq[FunctionEq]] = transNeededProps
      override val recursiveFunctions: Map[FunctionDef, DataType] = transRecursiveFuncs
      override val groupings: Seq[Seq[FunctionEq]] = transGroupings
      override val distinctionsBasedOnExpressions: Map[FunctionEq, Seq[FunctionExpJudgment]] = transDistinctions
      override val typesOfMetaVars: Map[(TypingRule, String), DataType] = transMetaVarTypes
      override val expressions: Seq[DataType] = transExprs
      override val contexts: Seq[DataType] = transCtxs
      override val types: Seq[DataType] = transTypes
    }
  }

  def translateAttachedProperties(): Map[(FunctionDef, String), TypingRule] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val ensuringFunctionTranslator = EnsuringFunctionTranslator(reporter)

    attachedProperties.map { case ((fdef, name), typing) =>
      (functionTranslator.translate(fdef), name) -> ensuringFunctionTranslator.translate(typing)
    }.toMap
  }

  def translatePropertiesNeeded(): Map[TypingRule, Seq[FunctionEq]] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val ensuringFunctionTranslator = EnsuringFunctionTranslator(reporter)
    propertyNeeded.map { case (typing, (fnname, cases)) =>
      val transTyping = ensuringFunctionTranslator.translate(typing)
      val transCases = cases.map { c => functionTranslator.translateCase(fnname, c) }
      transTyping -> transCases
    }.toMap
  }

  def translateRecursiveFunctions(): Map[FunctionDef, DataType] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    recursiveFunctions.map { case (fn, tr) =>
        val cases = adts(tr)
        functionTranslator.translate(fn) -> adtTranslator.translate(tr, cases)
    }.toMap
  }

  def translateGrouping(): Seq[Seq[FunctionEq]] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    groupings.map { case (name, eqs) =>
      eqs.map { eq => functionTranslator.translateCase(name, eq) }
    }
  }

  def translateDistinctions(): Map[FunctionEq, Seq[FunctionExpJudgment]] = {
    val functionTranslator = FunctionDefinitionTranslator(reporter, adts)
    val criteriaTranslator = DistinctionCriteriaTranslator(reporter)
    distinction.map { case ((name, cas), criterias) =>
      val transCriterias = criterias.map { c => criteriaTranslator.translate(c) }
      functionTranslator.translateCase(name, cas) -> transCriterias
    }.toMap
  }

  def translateTypesOfMetaVars(): Map[(TypingRule, String), DataType] = {
    val ensuringFuncTranslator = EnsuringFunctionTranslator(reporter)
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    typesOfMetaVars.map { case ((fn, name), tr) =>
      val typingRule = ensuringFuncTranslator.translate(fn)
      val cases = adts(tr)
      val dataType = adtTranslator.translate(tr, cases)
      (typingRule, name) -> dataType
    }.toMap
  }

  def translateCategory(traits: Seq[Defn.Trait]): Seq[DataType] = {
    val adtTranslator = AlgebraicDataTypeTranslator(reporter)
    traits.map { tr =>
      val cases = adts(tr)
      adtTranslator.translate(tr, cases)
    }
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