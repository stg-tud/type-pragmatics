package de.tu_darmstadt.veritas.newinputdsl.util

import scala.meta._

trait ADTCollector {
  def reporter: Reporter

  def collectADTs(parsed: Seq[Stat]): Map[Defn.Trait, Seq[Defn.Class]] = {
    val caseClasses = collectCaseClasses(parsed)
    val traits = collectBaseTraits(parsed)
    val illegalCaseClasses = findIllegalCaseClasses(parsed)
    if (illegalCaseClasses)
      reporter.report("A case class has no base trait or does not inherit from a trait that was defined with the object")
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
      ScalaMetaUtils.containsAnnotation(tr.mods, "Local")
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
}

object ADTCollector {
  def apply(): ADTCollector = new ADTCollector {
    override def reporter = Reporter()
  }
}