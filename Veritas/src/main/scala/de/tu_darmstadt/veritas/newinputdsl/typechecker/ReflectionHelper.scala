package de.tu_darmstadt.veritas.newinputdsl.typechecker
import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api
import scala.reflect.api.TypeCreator
import scala.reflect.api.Universe

object ReflectionHelper {
  // TODO how do we handle underspecified data types? Should this be even possible?
  def execute(spec: SPLSpecification, exprString: String): Any = {
    val specPath = spec.getClass.getName
    val specTypeTag = createTypeTagFromString(specPath)
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val (name, args) = extractNameAndArgs(exprString)
    val argsInstances = args.map { arg =>
      execute(spec, arg)
    }

    val symbol = specTypeTag.tpe.decl(ru.TermName(name))
    if (symbol.isMethod) {
      val function = symbol.asMethod
      val instanceMirror = mirror.reflect(spec)
      val functionMirror = instanceMirror.reflectMethod(function)
      functionMirror(argsInstances: _*)
    } else {
      val ctorTypeTag = createTypeTagFromString(s"$specPath$name")
      val ctor = ctorTypeTag.tpe.decl(ru.termNames.CONSTRUCTOR).asMethod
      val classMirror = mirror.reflectClass(ctorTypeTag.tpe.typeSymbol.asClass)
      val ctorMirror = classMirror.reflectConstructor(ctor)
      ctorMirror(argsInstances: _*)
    }
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
