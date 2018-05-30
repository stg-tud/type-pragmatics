package de.tu_darmstadt.veritas.scalaspl.typechecker
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api
import scala.reflect.api.TypeCreator
import scala.reflect.api.Universe

object ReflectionHelper {
  // idea is to register terms of an underspecified data type before running an execution or typecheck
  private val openTermMap = mutable.Map[String, AnyRef]()

  def clearOpenTermMap(): Unit = openTermMap.clear()

  def registerTerm(term: AnyRef): Unit = {
    val unwrapped = term.toString.replaceAll("\"", "")
    openTermMap(unwrapped) = term
  }

  def executeFunctionExp(f: FunctionExpMeta)(implicit specPath: String, substs: Map[String, Any]): Any = f match {
    case app: FunctionExpApp if app.functionName.startsWith("ODTRef_") && app.args.isEmpty =>
      // lookup registered term if app contains the prefix
      val stripped = app.functionName.replaceFirst("ODTRef_", "")
      openTermMap(stripped)
    case app: FunctionExpApp =>
      // execute args first
      val executedArgs = app.args.map { executeFunctionExp }
      ReflectionHelper.execute(specPath, app.functionName, executedArgs)
    case FunctionExpNot(inner) =>
      !executeFunctionExp(inner).asInstanceOf[Boolean]
    case FunctionExpAnd(l, r) =>
      val executedLeft = executeFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = executeFunctionExp(r).asInstanceOf[Boolean]
      executedLeft && executedRight
    case FunctionExpOr(l, r) =>
      val executedLeft = executeFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = executeFunctionExp(r).asInstanceOf[Boolean]
      executedLeft || executedRight
    case FunctionExpBiImpl(l, r) =>
      val executedLeft = executeFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = executeFunctionExp(r).asInstanceOf[Boolean]
      (executedLeft && executedRight) || (!executedLeft && !executedRight)
    case FunctionExpEq(l, r) =>
      val executedLeft = executeFunctionExp(l)
      val executedRight = executeFunctionExp(r)
      executedLeft == executedRight
    case FunctionExpNeq(l, r) =>
      val executedLeft = executeFunctionExp(l)
      val executedRight = executeFunctionExp(r)
      executedLeft != executedRight
    case FunctionExpIf(cond, thn, els) =>
      val executedCond = executeFunctionExp(cond).asInstanceOf[Boolean]
      if (executedCond) executeFunctionExp(thn)
      else executeFunctionExp(els)
    case FunctionExpLet(name, named, in) =>
      val evalNamed = executeFunctionExp(named)
      executeFunctionExp(in)(specPath, substs + (name -> evalNamed))
    case FunctionExpTrue => true
    case FunctionExpFalse => false
    case FunctionExpVar(name) => substs(name)
    case FunctionMeta(MetaVar(name)) =>
      throw new IllegalArgumentException("Function expression should not contain any metavariables")
  }

  def registerTerms(terms: AnyRef*): Unit = {
    terms.foreach {
      registerTerm
    }
  }

  def termRegistered(key: String): Boolean = {
    val unwrapped = key.replaceAll("\"", "")
    openTermMap.contains(unwrapped)
  }

  def execute(specPath: String, functionName: String, args: Seq[Any]): Any = {
    val prepSpecPath =
      if (specPath.endsWith("$")) specPath
      else specPath + "$"
    val specTypeTag = createTypeTagFromString(prepSpecPath)
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val symbol = specTypeTag.tpe.decl(ru.TermName(functionName))
    if (symbol.isMethod) {
      val function = symbol.asMethod
      val specObject = getObjectByPath(prepSpecPath)
      val instanceMirror = mirror.reflect(specObject)
      val functionMirror = instanceMirror.reflectMethod(function)
      functionMirror(args: _*)
    } else {
      val ctorTypeTag = createTypeTagFromString(s"$prepSpecPath$functionName")
      val ctor = ctorTypeTag.tpe.decl(ru.termNames.CONSTRUCTOR).asMethod
      val classMirror = mirror.reflectClass(ctorTypeTag.tpe.typeSymbol.asClass)
      val ctorMirror = classMirror.reflectConstructor(ctor)
      ctorMirror(args: _*)
    }
  }

  def execute(specPath: String, exprString: String): Any = {
    val (name, args) = extractNameAndArgs(exprString)
    val argsInstances = args.map { arg =>
      execute(specPath, arg)
    }
    execute(specPath, name, argsInstances)
  }

  private def extractNameAndArgs(str: String): (String, Seq[String]) = {
    // assume we only use function applications
    val index = str.indexOf("(")
    if (index == -1)
      return (str, Seq())
    val ctorName = str.substring(0, index)
    val strInsideParens = str.substring(index + 1, str.length - 1)
    (ctorName, getTopLevelArgs(strInsideParens))
  }

  private def getObjectByPath(path: String): AnyRef = {
    val mirror = runtimeMirror(getClass.getClassLoader)
    val module = mirror.staticModule(path)
    mirror.reflectModule(module).instance.asInstanceOf[AnyRef]
  }

  private def getTopLevelArgs(str: String): Seq[String] = {
    var unmatchedParens = 0
    val result = ListBuffer[String]()
    var arg = ""
    // remove whitespace
    for (char <- str.replace(" ", "")) {
      if (char == '(')
        unmatchedParens += 1
      if (char == ')')
        unmatchedParens -= 1
      if (char == ',' && unmatchedParens == 0) {
        result += arg
        arg = ""
      } else arg += char
    }
    if (arg.nonEmpty) result += arg
    result
  }

  // from https://stackoverflow.com/questions/23785439/getting-typetag-from-a-classname-string
  private def createTypeTagFromString[T](className: String): TypeTag[T] = {
    val c = Class.forName(className)
    val mirror = runtimeMirror(c.getClassLoader)
    val classSymbol = mirror.staticClass(className)
    val typ = classSymbol.selfType
    TypeTag(mirror, new TypeCreator {
      override def apply[U <: Universe with Singleton](m: api.Mirror[U]): U#Type = {
        if (m eq mirror) typ.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
      }
    })
  }
}
