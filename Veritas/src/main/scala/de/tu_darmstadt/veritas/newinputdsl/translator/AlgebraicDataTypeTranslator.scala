package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, SortRef}

import scala.meta._

trait AlgebraicDataTypeTranslator {

  def reporter: Reporter

  def translateADT(base: Defn.Trait, cases: Seq[Defn.Class]): DataType = {
    val open = isOpen(base)
    val name = base.name.value
    val superclasses = base.templ.inits.map { _.tpe.toString }
    if (!checkBaseTraitSuperType(superclasses))
      reporter.report("Base trait of abstract data type does not extend from Expression, Context, Typ", base.pos.startLine)
    if (base.tparams.nonEmpty)
      reporter.report("Type Parameters are not allowed", base.pos.startLine)
    val constrs = cases.map { translateCaseClass }
    DataType(open, name, constrs)
  }

  private def checkBaseTraitSuperType(supertypes: Seq[String]): Boolean = supertypes.forall(!_.contains(SPLTranslator.predefTraits))

  private def isOpen(tr: Defn.Trait): Boolean = {
    ScalaMetaUtils.containsAnnotation(tr.mods, "Open")
  }

  private def translateCaseClass(cas: Defn.Class): DataTypeConstructor = {
    val name = cas.name.value
    // always one parameterlist because it is case class and has to have one
    if (cas.tparams.nonEmpty)
      reporter.report("Type Parameters are not allowed", cas.pos.startLine)
    // has to have a superclass which is not Expression, Context or Typ
    // TODO: needs to be a trait which was defined in the top level object
    if (cas.templ.inits.isEmpty)
      reporter.report("The case class has no base trait and therefore does not belong to an abstract datatype definition", cas.pos.startLine)
    val sortRefs = correctParamList(cas.ctor.paramss.head)
    DataTypeConstructor(name, sortRefs)
  }

  private def correctParamList(params: Seq[Term.Param]): Seq[SortRef] = {
    if (params.exists(_.decltpe.isEmpty))
      reporter.report("A parameter definition has no type defined")
    val sortRefs = params.map { param =>
      SortRef(param.decltpe.get.toString)
    }
    sortRefs
  }
}

object AlgebraicDataTypeTranslator {
  def apply(r: Reporter): AlgebraicDataTypeTranslator = {
    new AlgebraicDataTypeTranslator {
      override val reporter: Reporter = r
    }
  }
}
