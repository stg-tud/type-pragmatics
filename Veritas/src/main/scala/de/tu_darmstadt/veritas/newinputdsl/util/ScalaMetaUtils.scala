package de.tu_darmstadt.veritas.newinputdsl

import scala.meta._

object ScalaMetaUtils {
  def translate[T](sourceString: String)(fn: Defn.Object => T)  = {
    val parsedSource = sourceString.parse[Source]
    parsedSource.toEither match {
      case Left(error) => throw error.details
      case Right(source) =>
        collectTopLevelObject(source) match {
          case Some(o) => fn(o)
          case None => Reporter().report("The top level construct is not an object")
        }
    }
  }

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

  def notContainsAnnotation(mods: Seq[Mod], annotation: String): Boolean = {
    val annots = collectAnnotations(mods)
    annots.forall { _.init.tpe.toString != annotation }
  }
}
