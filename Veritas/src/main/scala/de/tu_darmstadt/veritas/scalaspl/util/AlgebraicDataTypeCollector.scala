package de.tu_darmstadt.veritas.scalaspl.util

import scala.meta._

trait AlgebraicDataTypeCollector {
  def reporter: Reporter

  def collectADTs(parsed: Seq[Stat]): Map[Defn.Trait, Seq[Defn.Class]] = {
    val traits = collectBaseTraits(parsed)
    val caseClasses = collectCaseClasses(parsed)
    val illegalCaseClasses = findIllegalCaseClasses(traits, caseClasses)
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
  private def findIllegalCaseClasses(traits: Seq[Defn.Trait], caseClasses: Seq[Defn.Class]): Boolean = {

    val noSuperClass = caseClasses.exists { _.templ.inits.isEmpty }
    val noBaseTraitSuperClass = caseClasses.exists { cc =>
      val baseNames = traits.map { _.name.value }
      cc.templ.inits.exists{ n => !baseNames.contains(n.tpe.toString)}
    }

    noSuperClass || noBaseTraitSuperClass
  }
}

object AlgebraicDataTypeCollector {
  def apply(): AlgebraicDataTypeCollector = new AlgebraicDataTypeCollector {
    override def reporter = Reporter()
  }
}