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
              throw new IllegalArgumentException("A case class has no base trait or does not inherit from a trait that was defined with the object")
            translateObject(o)
          case None => throw new IllegalArgumentException("Toplevel Construct is not an object")
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
      val defs = translateStats(o.templ.stats)
      Module(moduleName, Seq(), defs)
    } else
      throw new IllegalArgumentException("Object does not inherit from SPLSpecification")
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
      throw new IllegalArgumentException("Base trait of abstract data type does not extend from Expression, Context, Typ")
    if (base.tparams.nonEmpty)
      throw new IllegalArgumentException("Type Parameters are not allowed")
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
      throw new IllegalArgumentException("Type Parameters are not allowed")
    // has to have a superclass which is not Expression, Context or Typ
    // TODO: needs to be a trait which was defined in the top level object
    if (cas.templ.inits.isEmpty)
      throw new IllegalArgumentException("The case class has no base trait and therefore does not belong to an abstract datatype definition")
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
      throw new IllegalArgumentException("The return type of a function has to be explicitly defined")
    if (fn.tparams.nonEmpty)
      throw new IllegalArgumentException("A function definition does not allow type parameters")
    val signature = translateFunctionSignature(fn.name, fn.paramss.head, fn.decltpe.get)
    val equations = fn.body match {
        // TODO: check that the expr over which is matched is a tuple in the correct order of function params
      case Term.Match(expr, cases) =>
        cases.map { translateCase(fn.name.value, _) }
      case _ =>
        throw new IllegalArgumentException("Toplevel construct of a function has to be a match")
    }
    FunctionDef(signature, equations)
  }

  private def translateFunctionSignature(name: Term.Name, params: Seq[Term.Param], returnType: Type): FunctionSig = {
    val sortRefs = correctParamList(params)
    FunctionSig(name.value, sortRefs, SortRef(returnType.toString()))
  }

  private def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      throw new IllegalArgumentException("")
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
          throw new IllegalArgumentException("Only constructors of case classes defined within the object are allowed")
        val args = pattern.map { translateInnerCasePattern }
        Seq(FunctionPatApp(term.toString, args))
      case Pat.Tuple(pattern) => pattern.map { translateInnerCasePattern }
      case Pat.Var(name) => Seq(FunctionPatVar(name.value))
      case _ => throw new IllegalArgumentException("Other pattern than tuple, application or variable is used")
    }
  }

  private def onlySelfDefinedCotrsUsed(name: String): Boolean = {
    adts.exists { case (base, cotrs) =>
        cotrs.exists { _.name.value == name}
    }
  }

  private def translateInnerCasePattern(pat: Pat): FunctionPattern = {
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        if (!onlySelfDefinedCotrsUsed(term.toString))
          throw new IllegalArgumentException("Only constructors of case classes defined within the object are allowed")
        FunctionPatApp(term.toString, args)
      case Pat.Var(name) => FunctionPatVar(name.value)
      case _ => throw new IllegalArgumentException("Other pattern than application or variable is used")
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
      throw new IllegalArgumentException("The return type of a function has to be explicitly defined")
    if (fn.tparams.nonEmpty)
      throw new IllegalArgumentException("A function definition does not allow type parameters")
    if (fn.paramss.size > 1)
      throw new IllegalArgumentException("A function definition can only have one parameter list")
    val metaBindings = fn.paramss.headOption.getOrElse(Nil).map { _.name.value }
    fn.body match {
      case Term.ApplyInfix(lhs, name, nil, Seq(rhs)) if name.value == "ensuring" =>
        val premises = translateJudgmentBlock(lhs)(metaBindings)
        val conclusions = translateTypingRule(rhs)(metaBindings)
        TypingRule(fn.name.value, premises, Seq(conclusions))
      case _ => throw new IllegalArgumentException("Axioms/Lemmas and Goals need to have an ensuring clause")
    }
  }

  private def translateJudgmentBlock(body: Term)(implicit metaVars: Seq[String] = Seq()): Seq[TypingRuleJudgment] = body match {
    case Term.Block(inner) => inner.map { translateRequire(_)(metaVars) }
    case _ => throw new IllegalArgumentException("")
  }

  private def translateRequire(stat: Stat)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = stat match {
    case Term.Apply(name, arg::Nil) if name.toString == "require" => translateTypingRule(arg)(metaVars)
    case _ => throw new IllegalArgumentException("Inside Axioms/Lemmas/Goals only require statements can be used")
  }

  private def translateTypingRule(term: Term)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = {
    val funTranslator = FunctionTranslator(metaVars)
    term match {
      case Lit.Boolean(true) => FunctionExpJudgment(FunctionExpTrue)
      case Lit.Boolean(false) => FunctionExpJudgment(FunctionExpFalse)
      case Term.Apply(name, arg::Nil)  if name.toString == "forall" =>
        val (vars, body) = translateQuantifiedExpr(arg)
        ForallJudgment(vars, Seq(body))
      case Term.Apply(name, arg::Nil)  if name.toString == "exists" =>
        val (vars, body) = translateQuantifiedExpr(arg)
        ExistsJudgment(vars, Seq(body))
      case Term.Apply(name, args) => FunctionExpJudgment(funTranslator.translateExp(term))
      case Term.ApplyInfix(lhs, name, Nil, arg::Nil) =>
        name.value match {
          case "::" => TypingJudgmentSimple(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(arg))
          case "|-" => arg match {
            case Term.ApplyInfix(inner, name, Nil, rhs::Nil) if name.value == "::" =>
              TypingJudgment(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(inner), funTranslator.translateExpMeta(rhs))
            case _ => throw new IllegalArgumentException("")
          }
          case "||" => throw new IllegalArgumentException("Needs to be implemented")// OrJudgment
          case _ => throw new IllegalArgumentException("Unsupported infix operation")
        }
    }
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
      throw new IllegalArgumentException("A local block can not extend another trait")
    val valBlocks = collectValBlocks(block.templ.stats)
    val translatedValBlocks = valBlocks.map { case (block, different) => translateConstantBlock(block, different) }
    Local(translateStats(block.templ.stats) ++ translatedValBlocks)
  }

  private def collectValBlocks(stats: Seq[Stat]): Seq[(Seq[Decl.Val], Boolean)] = {
    val result = ListBuffer[(ListBuffer[Decl.Val], Boolean)]()
    stats.foreach {
      case _: Defn.Val => throw new IllegalArgumentException("Defintion of vals are not allowed")
      case v: Decl.Val if containsAnnotation(v.mods, "Different") =>
        result += ListBuffer[Decl.Val]() -> true
      case v: Decl.Val =>
        if (result.last._2)
          result.last._1 += v
        else
          result += ListBuffer[Decl.Val]() -> false
      case _ =>
        result += ListBuffer[Decl.Val]() -> false
    }
    result.toList.map { block => (block._1.toList, block._2) }
  }

  private def translateConstantBlock(vals: Seq[Decl.Val], different: Boolean): Consts =
    Consts(vals.map { translateConstant }, different)

  private def translateConstant(v: Decl.Val): ConstDecl = {
    val constName = v.pats match {
      case Pat.Var(name) => name.value
      case _ => throw new IllegalArgumentException("Another pattern than simple variable assignment was used")
    }
    ConstDecl(constName, SortRef(v.decltpe.toString()))
  }
}

object SPLTranslator {
  val predefTraits = Seq("Expression", "Context", "Typ")

  val predefTypes = Seq("Bool", "iType")
}

case class FunctionTranslator(metavars: Seq[String]) {

  def translateExpMeta(term: Term): FunctionExpMeta = term match {
    case Term.Name(name) if metavars.contains(name) => FunctionMeta(MetaVar(name))
    case _ => translateExp(term)
  }

  def translateExp(term: Term): FunctionExp = term match {
    case Term.If(cond, then, els) => translateIf(cond, then, els)
    case Term.Block(stats) => translateBlock(stats)
    case Lit.Boolean(true) => FunctionExpTrue
    case Lit.Boolean(false) => FunctionExpFalse
    case Term.Apply(name, args) => translateApply(name, args)
    case Term.ApplyUnary(name, expr) =>
      if (name.value == "!")
        FunctionExpNot(translateExp(expr))
      else throw new IllegalArgumentException("This unary operator is not supported")
    case Term.ApplyInfix(lhs, name, nil, Seq(rhs)) => translateApplyInfix(lhs, name, rhs)
    case Term.Name(name) => FunctionExpVar(name)
    case _ => throw new IllegalArgumentException("")
  }

  private def translateIf(cond: Term, then: Term, els: Term): FunctionExp =
    FunctionExpIf(translateExp(cond), translateExpMeta(then), translateExpMeta(els))

  private def translateApply(name: Term, args: Seq[Term]): FunctionExp = {
    val transArgs = args.map { translateExpMeta }
    FunctionExpApp(name.toString, transArgs)
  }

  private def translateBlock(stats: Seq[Stat]): FunctionExpLet = {
    val bindings = stats.init.map {
      case Defn.Val(Seq(), Seq(Pat.Var(name)), None, rhs) =>
        (name, translateExpMeta(rhs))
      case _ => throw new IllegalArgumentException("")
    }
    val in = stats.last match {
      case expr: Term => translateExpMeta(expr)
      case _ => throw new IllegalArgumentException("Last term of block is not an expression")
    }
    var let = in
    bindings.reverse.foreach { case (name, expr) =>
       let = FunctionExpLet(name.value, expr, let)
    }
    let.asInstanceOf[FunctionExpLet]
  }

  // assume that we have left assoc operators (&& ||) in spoofax it was right but that seems counterintuitive
  private def translateApplyInfix(lhs: Term, name: Term.Name, rhs: Term): FunctionExp = name.value match {
    case "==" => FunctionExpEq(translateExpMeta(lhs), translateExpMeta(rhs))
    case "!=" => FunctionExpNeq(translateExpMeta(lhs), translateExpMeta(rhs))
    case "&&" => FunctionExpAnd(translateExp(lhs), translateExp(rhs))
    case "||" => FunctionExpOr(translateExp(lhs), translateExp(rhs))
    case "<==>" => FunctionExpBiImpl(translateExp(lhs), translateExp(rhs))
    case _ => throw new IllegalArgumentException("Unsupported operator was used")
  }
}