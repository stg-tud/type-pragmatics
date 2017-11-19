package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.meta._

trait DomainSpecificKnowledge {
  def simpleRecursiveFunctions(): Functions
}

class SPLTranslator {
  def translate(sourceFile: File): Module = {
    val parsedSource = sourceFile.parse[Source]
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

  def collectTopLevelObject(source: Source): Option[Defn.Object] = source.collect {
    case o: Defn.Object => o
  }.headOption

  // TODO see how we could track where the error occured
  def translateObject(o: Defn.Object): Module = {
      // check if it extends SPLSpecification
      if (o.templ.inits.nonEmpty && o.templ.inits.head.tpe.toString == "SPLSpecification") {
        val moduleName = o.name.value
        val adts = collectADTs(o.templ.stats)
        val translatedDataTypes = adts.map { case (base, cases) => translateADT(base, cases) }
        val functions = collectFunctions(o.templ.stats)
        val translatedFunctions = functions.map { translateFunction }
        val defs: Seq[ModuleDef] = translatedDataTypes.toSeq ++ Seq(Functions(translatedFunctions))
        Module(moduleName, Seq(), defs)
      } else
        throw new IllegalArgumentException("Object does not inherit from SPLSpecification")
    }

  def collectADTs(parsed: Seq[Stat]): Map[Defn.Trait, Seq[Defn.Class]] = {
    val caseClasses = collectCaseClasses(parsed)
    val traits = collectBaseTraits(parsed)
    traits.map { tr =>
      val subclasses = caseClasses.filter { cc =>
        cc.templ.inits.headOption.exists(_.tpe.toString == tr.name.value)
      }
      (tr, subclasses)
    }.toMap
  }

  def collectCaseClasses(parsed: Seq[Stat]): Seq[Defn.Class] = parsed.collect {
    case cc: Defn.Class if cc.mods.head.is[Mod.Case] => cc
  }

  def collectBaseTraits(parsed: Seq[Stat]): Seq[Defn.Trait] = parsed.collect {
    case tr: Defn.Trait => tr }.filterNot { tr =>
    containsAnnotation(tr.mods, "Local")
  }

  // check if a case class has no defined superclass or a superclass is not a within the object defined trait
  def findIllegalCaseClasses(parsed: Seq[Stat]): Boolean = {
    val baseTraits = collectBaseTraits(parsed)
    val caseClasses = collectCaseClasses(parsed)

    val noSuperClass = caseClasses.exists { _.templ.inits.isEmpty }
    val noBaseTraitSuperClass = caseClasses.exists { cc =>
      val baseNames = baseTraits.map { _.name.value }
      cc.templ.inits.exists{ n => !baseNames.contains(n.tpe.toString)}
    }

    noSuperClass || noBaseTraitSuperClass
  }

  def translateADT(base: Defn.Trait, cases: Seq[Defn.Class]): DataType = {
    val open = isOpen(base)
    val name = base.name.value
    // TODO check that basetrait inherits from Expression, Context or Domain
    val superclasses = base.templ.inits.map { _.tpe.toString }
    if (!checkBaseTraitSuperType(superclasses))
      throw new IllegalArgumentException("Base trait of abstract data type does not extend from Expression, Context, Typ")
    if (base.tparams.nonEmpty)
      throw new IllegalArgumentException("Type Parameters are not allowed")
    val constrs = cases.map { translateCaseClass }
    DataType(open, name, constrs)
  }

  def checkBaseTraitSuperType(supertypes: Seq[String]): Boolean = supertypes.forall(!_.contains(SPLTranslator.predefTraits))

  def isOpen(tr: Defn.Trait): Boolean = {
    containsAnnotation(tr.mods, "Open")
  }

  def containsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annotations = mods.collect {
      case annot: Mod.Annot => annot
    }
    annotations.exists { _.init.tpe.toString == annotation }
  }

  def translateCaseClass(cas: Defn.Class): DataTypeConstructor = {
    val name = cas.name.value
    // always one parameterlist because it is case class and has to have one
    if (cas.tparams.nonEmpty)
      throw new IllegalArgumentException("Type Parameters are not allowed")
    // has to have a superclass which is not Expression, Context or Typ
    // TODO: needs to be a trait which was defined in the top level object
    if (cas.templ.inits.isEmpty)
      throw new IllegalArgumentException("The case class has no base trait and therefore does not belong to an abstract datatype definition")
    val inits = cas.templ.inits.map { _.tpe.toString }
    if (inits.contains("Expression", "Context", "Typ"))
      throw new IllegalArgumentException("The case class has no base trait and therefore does not belong to an abstract datatype definition")
    val sortRefs = correctParamList(cas.ctor.paramss.head)
    DataTypeConstructor(name, sortRefs)
  }

  def collectFunctions(parsed: Seq[Stat]): Seq[Defn.Def] =
    parsed.collect {
      // has no goal, axiom, lemma annotation
      case fn: Defn.Def if fn.mods.isEmpty => fn
    }

  def translateFunction(fn: Defn.Def): FunctionDef = {
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

  def translateFunctionSignature(name: Term.Name, params: Seq[Term.Param], returnType: Type): FunctionSig = {
    val sortRefs = correctParamList(params)
    FunctionSig(name.value, sortRefs, SortRef(returnType.toString()))
  }

  def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      throw new IllegalArgumentException("")
    val sortRefs = params.map { param =>
      SortRef(param.decltpe.get.toString)
    }
    sortRefs
  }

  def translateCase(funName: String, cas: Case): FunctionEq = {
    val patterns = translateCasePattern(cas.pat)
    val body = translateCaseExp(cas.body)
    FunctionEq(funName, patterns, body)
  }

  def translateCasePattern(pat: Pat): Seq[FunctionPattern] = {
    // TODO do i need to check that only constructos of our adts are used?
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        Seq(FunctionPatApp(term.toString, args))
      case Pat.Tuple(pattern) => pattern.map { translateInnerCasePattern }
      case Pat.Var(name) => Seq(FunctionPatVar(name.value))
      case _ => throw new IllegalArgumentException("Other pattern than tuple, application or variable is used")
    }
  }

  def translateInnerCasePattern(pat: Pat): FunctionPattern = {
    // TODO do i need to check that only constructos of our adts are used?
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        FunctionPatApp(term.toString, args)
      case Pat.Var(name) => FunctionPatVar(name.value)
      case _ => throw new IllegalArgumentException("Other pattern than application or variable is used")
    }
  }

  def translateCaseExp(body: Term): FunctionExp =
    body match {
        // let, true, false
      case Term.If(cond, then, els) => translateIf(cond, then, els)
      case Term.Apply(name, args) =>  translateApply(name, args)
      case Term.ApplyInfix(lhs, name, nil, Seq(rhs)) => translateApplyInfix(lhs, name, rhs)
      case Term.Block(stats) => translateBlock(stats)
      case Term.ApplyUnary(name, expr) =>
        if (name.value == "!")
          FunctionExpNot(translateCaseExp(expr))
        else throw new IllegalArgumentException("This unary operator is not supported")
      case Term.Name(name) => FunctionExpVar(name)
      case _ => throw new IllegalArgumentException("Construct is not supported in function bodies")
    }

  def translateBlock(stats: Seq[Stat]): FunctionExp = {
    val bindings = stats.init.map {
      case Defn.Val(Seq(), Seq(Pat.Var(name)), None, rhs) =>
        (name, translateCaseExp(rhs))
      case _ => throw new IllegalArgumentException("")
    }
    val in = stats.last match {
      case expr: Term => translateCaseExp(expr)
      case _ => throw new IllegalArgumentException("Last term of block is not an expression")
    }
    bindings.foldRight(in){ case ((name, binding), res) =>
      FunctionExpLet(name.value, binding, res)
    }
  }

  def translateIf(cond: Term, then: Term, els: Term): FunctionExpIf = FunctionExpIf(translateCaseExp(cond), translateCaseExp(then), translateCaseExp(els))

  def translateApply(name: Term, args: Seq[Term]): FunctionExpApp = {
    val transArgs = args.map { translateCaseExp }
    FunctionExpApp(name.toString, transArgs)
  }

  // assume that we have left assoc operators (&& ||) in spoofax it was right but that seems counterintuitive
  def translateApplyInfix(lhs: Term, name: Term.Name, rhs: Term): FunctionExp = {
    val transLhs = translateCaseExp(lhs)
    val transRhs = translateCaseExp(rhs)
    name.value match {
      // TODO: biimplication
      case "==" => FunctionExpEq(transLhs, transRhs)
      case "!=" => FunctionExpNeq(transLhs, transRhs)
      case "&&" => FunctionExpAnd(transLhs, transRhs)
      case "||" => FunctionExpOr(transLhs, transRhs)
      case "<==>" => FunctionExpBiImpl(transLhs, transRhs)
      case _ => throw new IllegalArgumentException("Unsupported operator was used")
    }
  }

  def translateName(name: String): FunctionExp = FunctionExpVar(name)

  // TODO: need parsing functions for all the possible Constructs

  def collectEnsuringFunctions(parsed: Seq[Stat]) = ???
  def collectEnsuringRequiringFunctions(parsed: Seq[Stat]) = ???

  def parseRequire(parsed: Parsed[Stat]): TypingRuleJudgment = ???
  def parseEnsuring(parsed: Parsed[Stat]): TypingRuleJudgment = ???

  def parseTypableSimple(parsed: Parsed[Stat]): TypingJudgmentSimple = ???
  def parseTypable(parsed: Parsed[Stat]): TypingJudgment = ???
  // TODO: do i need extra parsers for expju, existsju, forallju, orju, notju?

  def collectLocalBlocks(parsed: Seq[Stat]) = ???
  def parseLocal(parsed: Parsed[Stat]): Local = ???
  def parseConstant(parsed: Parsed[Stat]): ConstDecl = ???
  def parseGoal(parsed: Parsed[Stat]): TypingRule = ???
  def parseAxiom(parsed: Parsed[Stat]): TypingRule  = ???
  def parseLemma(parsed: Parsed[Stat]): TypingRule = ???
}

object SPLTranslator {
  val predefTraits = Seq("Expression", "Context", "Typ")
}
