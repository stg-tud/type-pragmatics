package de.tu_darmstadt.veritas.newinputdsl

import scala.annotation.StaticAnnotation

trait SPLDomainSpecifcKnowledgeAnnotations {
  // TODO same for preservation
  // TODO Should this just be generalized? goal/lemma?
  case class ProgressProperty(propertyName: String) extends scala.annotation.Annotation
  // trade-off to refer by position number because we cannot annotate a case
  case class ProgressNeeded(functionName: String, functionEquationPositions: Int*) extends scala.annotation.Annotation {
    require(functionEquationPositions.nonEmpty)
  }
  // TODO need to check that if progressneeded is used the function refered has the progressprop annotation

  // TODO what happens if reduce semantics takes two arguments that need to be reduced?
  // => One solution would be that designers would be need a tuple adt
  // first position function param pos, second ctor position of function param and so on
  case class Recursive(positions: Int*) extends scala.annotation.Annotation {
    require(positions.nonEmpty)
  }

  // check what is the common factor in those groups and create a case distinction for them
  // for example outer cotrs (qsingle, qseq, qcond)
  case class GroupedDistinction(positions: Seq[Int]*) extends scala.annotation.Annotation {
    val atLeastOneGroup = positions.nonEmpty
    val allGroupsNonEmpty = positions.seq.forall { group => group.nonEmpty }
    require(atLeastOneGroup && allGroupsNonEmpty)
  }

  // TODO how do we pass the distinction criteria?
  // these distinctions are equalities/inequalities
  // one possiblity could be defs which are referenced by name
  // TODO what if the distinction represents a grouping which were marked by GroupedDistinction?
  // just point to one of the elements in the grouping or the grouping itself?
  // By checking if a position is already grouped we know that the distinction has to be applied to a subobligation of
  // a structural distinction
  case class Distinction(position: Int, criteriaNames: String*) extends scala.annotation.Annotation {
    require(criteriaNames.nonEmpty)
  }

  // TODO how do we make a lemma app in a sub of a distinction?
  // by progress/lemma needed? and determine which criteria fits the function pattern?
}

trait SPLSpecification extends SPLDomainSpecifcKnowledgeAnnotations {
  case class Open() extends StaticAnnotation
  case class Different() extends StaticAnnotation
  case class Axiom() extends StaticAnnotation
  case class Lemma() extends StaticAnnotation
  case class Goal() extends StaticAnnotation
  case class Local() extends StaticAnnotation

  // case class StaticDomain() extends StaticAnnotation
  // case class DynamicDomain() extends StaticAnnotation
  // case class SimpleRecursive() extends StaticAnnotation
  // case class Transform() extends StaticAnnotation

  implicit class _Boolean(lhs: Boolean) {
    def <==> (rhs: Boolean): Boolean = biimplication(lhs, rhs)
    // TODO: need better notation maybe simple &&?
    def $$ (rhs: Boolean): Boolean = lhs && rhs
  }

  def biimplication(lhs: Boolean, rhs: Boolean): Boolean = (!lhs || rhs) && (!rhs || lhs)

  // shortcomings: is not executable. But we dont even know yet what it means to have exectuable axioms / lemmas
  def forall[T1](fun: Function1[T1, Boolean]): Boolean = true
  def forall[T1, T2](fun: Function2[T1, T2, Boolean]): Boolean = true
  def forall[T1, T2, T3](fun: Function3[T1, T2, T3, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4](fun: Function4[T1, T2, T3, T4, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5](fun: Function5[T1, T2, T3, T4, T5, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5, T6](fun: Function6[T1, T2, T3, T4, T5, T6, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5, T6, T7](fun: Function7[T1, T2, T3, T4, T5, T6, T7, Boolean]): Boolean = true

  def exists[T1](fun: Function1[T1, Boolean]): Boolean = true
  def exists[T1, T2](fun: Function2[T1, T2, Boolean]): Boolean = true
  def exists[T1, T2, T3](fun: Function3[T1, T2, T3, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4](fun: Function4[T1, T2, T3, T4, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5](fun: Function5[T1, T2, T3, T4, T5, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5, T6](fun: Function6[T1, T2, T3, T4, T5, T6, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5, T6, T7](fun: Function7[T1, T2, T3, T4, T5, T6, T7, Boolean]): Boolean = true

  // every expression has to be a subclass of this trait
  // Therefore we can can determine automatically which params belong to the expression domain
  trait Expression
  trait Context
  trait Typ

  // implicits for easier notation
  // Context |- Expression :: Typ or Expression :: Typ
  implicit class _Expression(expr: Expression) {
    def :: (typ: Typ): Boolean = typable(expr, typ)
  }

  implicit class _Typ(typ: Typ) {
    def :: (expr: Expression): _ExprTypBinding = _ExprTypBinding((expr, typ))
  }

  implicit class _Context(context: Context) {
    def |- (binding: _ExprTypBinding): Boolean = typable(context, binding.binding._1, binding.binding._2)
  }

  implicit class _ExprTypBinding(val binding: (Expression, Typ))

  implicit def toBoolean(binding: _ExprTypBinding): Boolean = typable(binding.binding._1, binding.binding._2)

  def typable(context: Context, exp: Expression, typ: Typ): Boolean
  def typable(exp: Expression, typ: Typ): Boolean
}
