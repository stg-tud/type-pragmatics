package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.meta._
import scala.util.{Failure, Success, Try}

trait DomainSpecificKnowledge {

}

class SPLTranslator {
  def translate(sourceFile: File): Try[Module] = {
    val parsedSource = sourceFile.parse[Source]
    parsedSource.toEither match {
      case Left(error) => Failure(error.details)
      case Right(source) =>
        collectTopLevelObject(source) match {
          case Some(o) =>
            val illegalCaseClasses = findIllegalCaseClasses(o.templ.stats)
            if (illegalCaseClasses)
              return Failure(new IllegalArgumentException("A case class has no base trait or does not inherit from a trait that was defined with the object"))
            translateObject(o)
          case None => Failure(new IllegalArgumentException("Toplevel Construct is not an object"))
        }

    }
  }

  def collectTopLevelObject(source: Source): Option[Defn.Object] = source.collect {
    case o: Defn.Object => o
  }.headOption

  // TODO see how we could track where the error occured
  def translateObject(o: Defn.Object): Try[Module] = {
      // check if it extends SPLSpecification
      if (o.templ.inits.nonEmpty && o.templ.inits.head.tpe.toString == "SPLSpecification") {
        val moduleName = o.name.value
        val adts = collectADTs(o.templ.stats)
        val translatedDataTypes = adts.map { case (base, cases) => translateADT(base, cases) }
        val functions = collectFunctions(o.templ.stats)
        val translatedFunctions = functions.map { translateFunction }
        val failure = getFailure(translatedDataTypes.toSeq ++ translatedFunctions)
        if (failure.isEmpty) {
          val defs: Seq[ModuleDef] = translatedDataTypes.map { _.get }.toSeq ++ Seq(Functions(translatedFunctions.map { _.get }))
          Success(Module(moduleName, Seq(), defs))
        } else {
          Failure(failure.get)
        }
      } else
        Failure(new IllegalArgumentException("Object does not inherit from SPLSpecification"))
    }

  def getFailure[T](seq: Seq[Try[T]]): Option[Throwable] = {
    seq.find( _.isFailure).map { _.failed.get }
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

  def translateADT(base: Defn.Trait, cases: Seq[Defn.Class]): Try[DataType] = {
    val open = isOpen(base)
    val name = base.name.value
    // TODO check that basetrait inherits from Expression, Context or Domain
    val superclasses = base.templ.inits.map { _.tpe.toString }
    if (!checkBaseTraitSuperType(superclasses))
      return Failure(new IllegalArgumentException("Base trait of abstract data type does not extend from Expression, Context, Typ"))
    if (base.tparams.nonEmpty)
      return Failure(new IllegalArgumentException("Type Parameters are not allowed"))
    val constrs = cases.map { translateCaseClass }
    swapFailure(constrs) match  {
      case Success(cotrs) => Success(DataType(open, name, cotrs))
      case Failure(e) => Failure(e)
    }
  }

  def checkBaseTraitSuperType(supertypes: Seq[String]): Boolean = supertypes.forall(!_.contains(SPLTranslator.predefTraits))

  def swapFailure[T](attemps: Seq[Try[T]]): Try[Seq[T]] = {
    val failure = attemps.find { _.isFailure }
    failure match {
      case None => Success(attemps.map { _.get })
      case Some(Failure(e)) => Failure(e)
      case _ => null // cannot happen
    }
  }

  def isOpen(tr: Defn.Trait): Boolean = {
    containsAnnotation(tr.mods, "Open")
  }

  def containsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annotations = mods.collect {
      case annot: Mod.Annot => annot
    }
    annotations.exists { _.init.tpe.toString == annotation }
  }

  def translateCaseClass(cas: Defn.Class): Try[DataTypeConstructor] = {
    val name = cas.name.value
    // always one parameterlist because it is case class and has to have one
    if (cas.tparams.nonEmpty)
      return Failure(new IllegalArgumentException("Type Parameters are not allowed"))
    // has to have a superclass which is not Expression, Context or Typ
    // TODO: needs to be a trait which was defined in the top level object
    if (cas.templ.inits.isEmpty)
      return Failure(new IllegalArgumentException("The case class has no base trait and therefore does not belong to an abstract datatype definition"))
    val inits = cas.templ.inits.map { _.tpe.toString }
    if (inits.contains("Expression", "Context", "Typ"))
      return Failure(new IllegalArgumentException("The case class has no base trait and therefore does not belong to an abstract datatype definition"))
    val sortRefs = correctParamList(cas.ctor.paramss.head)
    sortRefs match {
      case Success(sorts) => Success(DataTypeConstructor(name, sorts))
      case Failure(e) => Failure(e)
    }
  }

  def collectFunctions(parsed: Seq[Stat]): Seq[Defn.Def] =
    parsed.collect {
      // has no goal, axiom, lemma annotation
      case fn: Defn.Def if fn.mods.isEmpty => fn
    }

  def translateFunction(fn: Defn.Def): Try[FunctionDef] = {
    // decltype has to be given
    if (fn.decltpe.isEmpty)
      return Failure(new IllegalArgumentException("The return type of a function has to be explicitly defined"))
    val signature = translateFunctionSignature(fn.name, fn.paramss.head, fn.decltpe.get)
    signature match {
      case Success(sig) =>
        val equations =
          fn.body match {
              // TODO: check that the expr over which is matched is a tuple in the correct order of function params
            case Term.Match(expr, cases) =>
              cases.map { translateCase(fn.name.value, _) }
            case _ =>
              return Failure(new IllegalArgumentException("Toplevel construct of a function has to be a match"))
          }
        swapFailure(equations) match {
          case Success(eqs) =>
            Success(FunctionDef(sig, eqs))
          case Failure(e) => Failure(e)
        }
      case Failure(e) => Failure(e)
    }
  }

  def translateFunctionSignature(name: Term.Name, params: Seq[Term.Param], returnType: Type): Try[FunctionSig] = {
    val sortRefs = correctParamList(params)
    sortRefs match {
      case Success(sorts) => Success(FunctionSig(name.value, sorts, SortRef(returnType.toString())))
      case Failure(e) => Failure(e)
    }
  }

  def correctParamList(params: Seq[Term.Param]): Try[Seq[SortRef]] = {
    if (params.exists(_.decltpe.isEmpty))
      return Failure(null)
    val sortRefs = params.map { param =>
      SortRef(param.decltpe.get.toString)
    }
    Success(sortRefs)
  }

  def translateCase(funName: String, cas: Case): Try[FunctionEq] = {
    val patterns = translateCasePattern(cas.pat)
    patterns match {
      case Success(transPatterns) =>
        val body = translateCaseExp(cas.body)
        body match {
          case Success(right) =>
            Success(FunctionEq(funName, transPatterns, right))
          case Failure(e) => Failure(e)
        }
      case Failure(e) => Failure(e)
    }
  }

  def translateCasePattern(pat: Pat): Try[Seq[FunctionPattern]] = {
    // TODO do i need to check that only constructos of our adts are used?
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        swapFailure(args) match {
          case Success(transArgs) => Success(Seq(FunctionPatApp(term.toString, transArgs)))
          case Failure(e) => Failure(e)
        }
      case Pat.Tuple(pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        swapFailure(args) match {
          case Success(transArgs) => Success(transArgs)
          case Failure(e) => Failure(e)
        }
      case Pat.Var(name) => Success(Seq(FunctionPatVar(name.value)))
      case _ => Failure(new IllegalArgumentException("Other pattern than tuple, application or variable is used"))
    }
  }

  def translateInnerCasePattern(pat: Pat): Try[FunctionPattern] = {
    // TODO do i need to check that only constructos of our adts are used?
    pat match {
      case Pat.Extract(term, pattern) =>
        val args = pattern.map { translateInnerCasePattern }
        swapFailure(args) match {
          case Success(transArgs) => Success(FunctionPatApp(term.toString, transArgs))
          case Failure(e) => Failure(e)
      }
      case Pat.Var(name) => Success(FunctionPatVar(name.value))
      case _ => Failure(new IllegalArgumentException("Other pattern than application or variable is used"))
    }
  }

  def translateCaseExp(body: Term): Try[FunctionExp] =
    body match {
        // let, true, false
      case Term.If(cond, then, els) => translateIf(cond, then, els)
      case Term.Apply(name, args) =>  translateApply(name, args)
      case Term.ApplyInfix(lhs, name, nil, Seq(rhs)) => translateApplyInfix(lhs, name, rhs)
      case Term.Block(stats) => translateBlock(stats)
      case Term.ApplyUnary(name, expr) =>
        if (name.value == "!")
          translateCaseExp(expr) match {
            case Success(f) => Success(FunctionExpNot(f))
            case Failure(e) => Failure(e)
          }
        else Failure(new IllegalArgumentException("This unary operator is not supported"))
      case Term.Name(name) => Success(FunctionExpVar(name))
      case _ => Failure(new IllegalArgumentException("Construct is not supported in function bodies"))
  }

  def translateBlock(stats: Seq[Stat]): Try[FunctionExp] = {
    val bindings = stats.init.collect {
      case Defn.Val(Seq(), Seq(Pat.Var(name)), None, rhs) =>
        (name, translateCaseExp(rhs))
    }
    val failed = bindings.find(_._2.isFailure)
    if (failed.nonEmpty) {
      return failed.get._2.asInstanceOf[Failure[FunctionExp]]
    }
    val in = stats.last match {
      case expr: Term => translateCaseExp(expr)
      case _ => Failure(new IllegalArgumentException("Last term of block is not an expression"))
    }
    in match {
      case Success(in) =>
        val result = bindings.foldRight(in){ case ((name, binding), res) =>
            FunctionExpLet(name.value, binding.get, res)
        }
        Success(result)
      case Failure(e) => Failure(e)
    }
  }

  def translateIf(cond: Term, then: Term, els: Term): Try[FunctionExpIf] =
    translateCaseExp(cond) match {
      case Success(transCond) =>
        translateCaseExp(then) match {
          case Success(transThen) =>
            translateCaseExp(els) match {
              case Success(transEls) =>
                Success(FunctionExpIf(transCond, transThen, transEls))
              case Failure(e) => Failure(e)
            }
          case Failure(e) => Failure(e)
        }
      case Failure(e) => Failure(e)
    }

  def translateApply(name: Term, args: Seq[Term]): Try[FunctionExpApp] = {
    val transArgs = args.map { translateCaseExp }
    swapFailure(transArgs) match {
      case Success(transArgs) => Success(FunctionExpApp(name.toString, transArgs))
      case Failure(e) => Failure(e)
    }
  }

  // assume that we have left assoc operators (&& ||) in spoofax it was right but that seems counterintuitive
  def translateApplyInfix(lhs: Term, name: Term.Name, rhs: Term): Try[FunctionExp] =
    translateCaseExp(lhs) match {
      case Success(lhs) =>
        translateCaseExp(rhs) match {
          case Success(rhs) =>
            name.value match {
                // TODO: biimplication
              case "==" => Success(FunctionExpEq(lhs, rhs))
              case "!=" => Success(FunctionExpNeq(lhs, rhs))
              case "&&" => Success(FunctionExpAnd(lhs, rhs))
              case "||" => Success(FunctionExpOr(lhs, rhs))
              case _ => Failure(new IllegalArgumentException("Unsupported operator was used"))
            }
          case Failure(e) => Failure(e)
        }
      case Failure(e) => Failure(e)
    }

  def translateName(name: String): Try[FunctionExp] = Success(FunctionExpVar(name))

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
