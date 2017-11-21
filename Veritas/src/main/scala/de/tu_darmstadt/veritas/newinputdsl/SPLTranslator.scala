package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable.ListBuffer
import scala.meta._

trait DomainSpecificKnowledge {
  def simpleRecursiveFunctions(): Functions
}

class SPLTranslator {
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
            val illegalCaseClasses = findIllegalCaseClasses(o.templ.stats)
            if (illegalCaseClasses)
              Reporter().report("A case class has no base trait or does not inherit from a trait that was defined with the object")
            translateObject(o)
          case None => Reporter().report("The top level construct is not an object")
        }
    }
  }

  private def collectTopLevelObject(source: Source): Option[Defn.Object] = source.collect {
    case o: Defn.Object => o
  }.headOption

  // TODO see how we could track where the error occured
  private def translateObject(o: Defn.Object): Module = {
    // check if it extends SPLSpecification
    if (o.templ.inits.nonEmpty && o.templ.inits.head.tpe.toString == "SPLSpecification") {
      val moduleName = o.name.value
      val locals = collectLocalBlocks(o.templ.stats)
      val transLocals = locals.map { translateLocal }
      val defs = translateStats(o.templ.stats)
      Module(moduleName, Seq(), defs ++ transLocals)
    } else Reporter().report(s"Object ${o.name.value} does not inherit from the trait SPLSpecification")
  }

  private def translateStats(stats: Seq[Stat]): Seq[ModuleDef] = {
    adts = collectADTs(stats)
    val translatedDataTypes = adts.map { case (base, cases) => translateADT(base, cases) }
    val functions = collectFunctions(stats)
    val translatedFunctions = functions.map { translateFunction }
    val axioms = collectAxioms(stats)
    val translatedAxioms = axioms.map { translateEnsuringFunction }
    val lemmas = collectLemmas(stats)
    val translatedLemmas = lemmas.map { translateEnsuringFunction }
    val goals = collectGoals(stats)
    val translatedGoals = goals.map { translateEnsuringFunction }
    translatedDataTypes.toSeq ++
      Seq(Functions(translatedFunctions)) ++
      Seq(Axioms(translatedAxioms)) ++
      Seq(Lemmas(translatedLemmas, None)) ++
      Seq(Goals(translatedGoals, None))
  }

  private def collectADTs(parsed: Seq[Stat]): Map[Defn.Trait, Seq[Defn.Class]] = {
    val caseClasses = collectCaseClasses(parsed)
    val traits = collectBaseTraits(parsed)
    traits.map { tr =>
      val subclasses = caseClasses.filter { cc =>
        cc.templ.inits.headOption.exists(_.tpe.toString == tr.name.value)
      }
      (tr, subclasses)
    }.toMap
  }

  private def collectCaseClasses(parsed: Seq[Stat]): Seq[Defn.Class] = parsed.collect {
    case cc: Defn.Class if cc.mods.head.is[Mod.Case] => cc
  }

  private def collectBaseTraits(parsed: Seq[Stat]): Seq[Defn.Trait] = parsed.collect {
    case tr: Defn.Trait => tr }.filterNot { tr =>
    containsAnnotation(tr.mods, "Local")
  }

  // check if a case class has no defined superclass or a superclass is not a within the object defined trait
  private def findIllegalCaseClasses(parsed: Seq[Stat]): Boolean = {
    val baseTraits = collectBaseTraits(parsed)
    val caseClasses = collectCaseClasses(parsed)

    val noSuperClass = caseClasses.exists { _.templ.inits.isEmpty }
    val noBaseTraitSuperClass = caseClasses.exists { cc =>
      val baseNames = baseTraits.map { _.name.value }
      cc.templ.inits.exists{ n => !baseNames.contains(n.tpe.toString)}
    }

    noSuperClass || noBaseTraitSuperClass
  }

  private def translateADT(base: Defn.Trait, cases: Seq[Defn.Class]): DataType = {
    val open = isOpen(base)
    val name = base.name.value
    val superclasses = base.templ.inits.map { _.tpe.toString }
    if (!checkBaseTraitSuperType(superclasses))
      Reporter().report("Base trait of abstract data type does not extend from Expression, Context, Typ", base.pos.startLine)
    if (base.tparams.nonEmpty)
      Reporter().report("Type Parameters are not allowed", base.pos.startLine)
    val constrs = cases.map { translateCaseClass }
    DataType(open, name, constrs)
  }

  private def checkBaseTraitSuperType(supertypes: Seq[String]): Boolean = supertypes.forall(!_.contains(SPLTranslator.predefTraits))

  private def isOpen(tr: Defn.Trait): Boolean = {
    containsAnnotation(tr.mods, "Open")
  }

  private def containsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annotations = mods.collect {
      case annot: Mod.Annot => annot
    }
    annotations.exists { _.init.tpe.toString == annotation }
  }

  private def translateCaseClass(cas: Defn.Class): DataTypeConstructor = {
    val name = cas.name.value
    // always one parameterlist because it is case class and has to have one
    if (cas.tparams.nonEmpty)
      Reporter().report("Type Parameters are not allowed", cas.pos.startLine)
    // has to have a superclass which is not Expression, Context or Typ
    // TODO: needs to be a trait which was defined in the top level object
    if (cas.templ.inits.isEmpty)
      Reporter().report("The case class has no base trait and therefore does not belong to an abstract datatype definition", cas.pos.startLine)
    val sortRefs = correctParamList(cas.ctor.paramss.head)
    DataTypeConstructor(name, sortRefs)
  }

  private def collectFunctions(parsed: Seq[Stat]): Seq[Defn.Def] =
    parsed.collect {
      // has no goal, axiom, lemma annotation
      case fn: Defn.Def if fn.mods.isEmpty => fn
    }

  private def translateFunction(fn: Defn.Def): FunctionDef = {
    // decltype has to be given
    if (fn.decltpe.isEmpty)
      Reporter().report("The return type of a function has to be explicitly defined", fn.pos.startLine)
    if (fn.tparams.nonEmpty)
      Reporter().report("A function definition does not allow type parameters", fn.pos.startLine)
    val signature = translateFunctionSignature(fn.name, fn.paramss.head, fn.decltpe.get)
    val equations = fn.body match {
        // TODO: check that the expr over which is matched is a tuple in the correct order of function params
      case Term.Match(_, cases) =>
        cases.map { translateCase(fn.name.value, _) }
      case _ =>
        Reporter().report("Top level construct of a function has to be a match", fn.body.pos.startLine)
    }
    FunctionDef(signature, equations)
  }

  private def translateFunctionSignature(name: Term.Name, params: Seq[Term.Param], returnType: Type): FunctionSig = {
    val sortRefs = correctParamList(params)
    FunctionSig(name.value, sortRefs, SortRef(returnType.toString()))
  }

  private def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      Reporter().report("A parameter definition has no type defined")
    val sortRefs = params.map { param =>
      SortRef(param.decltpe.get.toString)
    }
    sortRefs
  }

  private def translateCase(funName: String, cas: Case): FunctionEq = {
    val patterns = translateCasePattern(cas.pat)
    val funTranslator = FunctionTranslator(Seq())
    val body = funTranslator.translateExp(cas.body)
    FunctionEq(funName, patterns, body)
  }

  private def translateCasePattern(pat: Pat): Seq[FunctionPattern] = {
    pat match {
      case Pat.Extract(term, pattern) =>
        if (!onlySelfDefinedCotrsUsed(term.toString))
          Reporter().report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        val args = pattern.map { translateInnerCasePattern }
        Seq(FunctionPatApp(term.toString, args))
      case Pat.Tuple(pattern) => pattern.map { translateInnerCasePattern }
      case Pat.Var(name) => Seq(FunctionPatVar(name.value))
      case _ => Reporter().report("Other pattern than tuple, application or variable is used", pat.pos.startLine)
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
          Reporter().report("Only constructors of case classes defined within the object are allowed", pat.pos.startLine)
        FunctionPatApp(term.toString, args)
      case Pat.Var(name) => FunctionPatVar(name.value)
      case _ => Reporter().report("Other pattern than application or variable is used", pat.pos.startLine)
    }
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

  private def translateEnsuringFunction(fn: Defn.Def): TypingRule = {
    if (fn.decltpe.isEmpty)
      Reporter().report("The return type of a function has to be explicitly defined", fn.pos.startLine)
    if (fn.tparams.nonEmpty)
      Reporter().report("A function definition does not allow type parameters", fn.pos.startLine)
    if (fn.paramss.size > 1)
      Reporter().report("A function definition can only have one parameter list", fn.pos.startLine)
    val metaBindings = fn.paramss.headOption.getOrElse(Nil).map { _.name.value }
    fn.body match {
      case Term.ApplyInfix(lhs, name, Nil, Seq(rhs)) if name.value == "ensuring" =>
        val premises = translateJudgmentBlock(lhs)(metaBindings)
        // TODO: need support for multiple conclusions
        val conclusions = translateTypingRule(rhs)(metaBindings)
        TypingRule(fn.name.value, premises, Seq(conclusions))
      case _ => Reporter().report("Axioms/Lemmas and Goals need to have an ensuring clause", fn.pos.startLine)
    }
  }

  private def translateJudgmentBlock(body: Term)(implicit metaVars: Seq[String] = Seq()): Seq[TypingRuleJudgment] = body match {
    case Term.Block(inner) => inner.map { translateRequire(_)(metaVars) }
    case _ => Reporter().report("")
  }

  private def translateRequire(stat: Stat)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = stat match {
    case Term.Apply(name, arg::Nil) if name.toString == "require" => translateTypingRule(arg)(metaVars)
    case _ => Reporter().report("Inside Axioms/Lemmas/Goals only require statements can be used", stat.pos.startLine)
  }

  private def translateTypingRule(term: Term)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = {
    val funTranslator = FunctionTranslator(metaVars)
    term match {
      case Lit.Boolean(true) => FunctionExpJudgment(FunctionExpTrue)
      case Lit.Boolean(false) => FunctionExpJudgment(FunctionExpFalse)
        // TODO support multiple bodies? Maybe via &&
      case Term.Apply(name, arg::Nil)  if name.toString == "forall" =>
        val (vars, body) = translateQuantifiedExpr(arg)
        ForallJudgment(vars, Seq(body))
      case Term.Apply(name, arg::Nil)  if name.toString == "exists" =>
        val (vars, body) = translateQuantifiedExpr(arg)
        ExistsJudgment(vars, Seq(body))
      case Term.Apply(_, _) => FunctionExpJudgment(funTranslator.translateExp(term))
      case Term.ApplyInfix(lhs, name, Nil, arg::Nil) =>
        name.value match {
          case "::" => TypingJudgmentSimple(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(arg))
          case "|-" => arg match {
            case Term.ApplyInfix(inner, Term.Name("::"), Nil, rhs::Nil) =>
              TypingJudgment(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(inner), funTranslator.translateExpMeta(rhs))
            case _ => Reporter().report("Invalid operator in typing statement", term.pos.startLine)
          }
          case "||" => translateOrEnsuring(lhs, arg, metaVars)
          case op => Reporter().report(s"Unsupported infix operation $op inside the ensuring statement", term.pos.startLine)
        }
    }
  }

  private def translateOrEnsuring(lhs: Term, rhs: Term, metavars: Seq[String]): OrJudgment = {
    val cases = ListBuffer[Seq[TypingRuleJudgment]]()
    def process(term: Term): Unit = term match {
      case Term.ApplyInfix(l, Term.Name("||"), Nil, r::Nil) =>
        process(l)
        process(r)
      case _ =>
        val funTranslator = FunctionTranslator(metavars)
        cases += Seq(FunctionExpJudgment(funTranslator.translateExp(term)))
    }
    process(lhs)
    process(rhs)
    OrJudgment(cases)
  }

  private def translateQuantifiedExpr(term: Term)(implicit metavars: Seq[String] = Seq()): (Seq[MetaVar], TypingRuleJudgment) = term match {
    case Term.Function(params, body) =>
      val quantifiedvars = params.map { p => MetaVar(p.name.value) }
      val quantifiednames = quantifiedvars.map {_.name}
      val funTranslator = FunctionTranslator(quantifiednames ++ metavars)
      (quantifiedvars, FunctionExpJudgment(funTranslator.translateExp(body)))
  }

  private def collectLocalBlocks(parsed: Seq[Stat]): Seq[Defn.Trait] = parsed.collect {
    case tr: Defn.Trait if containsAnnotation(tr.mods, "Local") => tr
  }

  private def translateLocal(block: Defn.Trait): Local = {
    // TODO: what do we need to check for?
    if(block.templ.inits.nonEmpty)
      Reporter().report("A local block can not extend another trait", block.pos.startLine)
    val valBlocks = collectValBlocks(block.templ.stats)
    val translatedValBlocks = valBlocks.map { case (vals, different) => translateConstantBlock(vals, different) }
    Local(translateStats(block.templ.stats) ++ translatedValBlocks)
  }

  private def collectValBlocks(stats: Seq[Stat]): Seq[(Seq[Decl.Val], Boolean)] = {
    val result = ListBuffer[(ListBuffer[Decl.Val], Boolean)]()
    stats.foreach {
      case v: Defn.Val => Reporter().report("Defintion of vals within a local block are not allowed", v.pos.startLine)
      case v: Decl.Val if containsAnnotation(v.mods, "Different") =>
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
      Reporter().report("Multiple variable declaration is not supported", v.pos.startLine)
    val constName = v.pats.head match {
      case Pat.Var(name) => name.value
      case _ => Reporter().report("Another pattern than simple variable assignment was used", v.pats.head.pos.startLine)
    }
    ConstDecl(constName, SortRef(v.decltpe.toString()))
  }
}

object SPLTranslator {
  val predefTraits = Seq("Expression", "Context", "Typ")

  val predefTypes = Seq("Bool", "iType")
}

