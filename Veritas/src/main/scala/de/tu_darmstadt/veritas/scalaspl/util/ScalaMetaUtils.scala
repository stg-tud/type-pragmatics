package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslationError

import scala.meta._

object ScalaMetaUtils {
  def getObjectPath(sourceString: String): String = {
    val source = parseString(sourceString)
    val packageString = collectPackage(source) match {
      case Some(pkg) => pkg.ref.toString()
      case None => ""
    }
    val objectName = collectTopLevelObject(source) match {
      case Some(obj) => obj.name.value
      case None => Reporter().report("The top level construct is not an object")
    }
    s"$packageString.$objectName"
  }

  def translate[T](sourceString: String)(fn: Defn.Object => T): T = {
    val source = parseString(sourceString)
    collectTopLevelObject(source) match {
      case Some(o) => fn(o)
      case None => Reporter().report("The top level construct is not an object")
    }
  }

  private def parseString(sourceString: String): Source = {
    val parsedSource = sourceString.parse[Source]
    parsedSource.toEither match {
      case Left(error) => throw error.details
      case Right(source) => source
    }
  }

  private def collectPackage(source: Source): Option[Pkg] = source.collect {
    case pgk: Pkg => pgk
  }.headOption

  def collectTopLevelObject(source: Source): Option[Defn.Object] = source.collect {
    case o: Defn.Object => o
  }.headOption

  def collectAnnotations(mods: Seq[Mod]): Seq[Mod.Annot] =
    mods.collect {
      case annot: Mod.Annot => annot
    }

  def containsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annots = collectAnnotations(mods)
    annots.exists { _.init.tpe.toString == annotation }
  }

  def containsOneOfAnnotations(mods: Seq[Mod], annotations: Seq[String]): Boolean = {
    val annots = collectAnnotations(mods)
    annots.exists { annot => annotations.contains(annot.init.tpe.toString)}
  }

  def notContainsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annots = collectAnnotations(mods)
    annots.forall { _.init.tpe.toString != annotation }
  }

  def notContainsOneAnnotation(mods: Seq[Mod], annotations: Seq[String]): Boolean = {
    val annots = collectAnnotations(mods)
    annots.forall { annot => !annotations.contains(annot.init.tpe.toString) }
  }

  // Converts string into a Term if possible
  def getTerm(expString: String): Term = {
    val parsed = expString.parse[Term]
    parsed.toEither match {
      case Left(_) =>
        throw ScalaSPLTranslationError(s"Could not parse the $expString into a Scalameta node of type Term")
      case Right(term) => term
    }
  }
}
