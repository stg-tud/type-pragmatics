package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.scalaspl.util.ScalaMetaUtils

trait ConstraintSolver {
  def solve(constraints: Set[FunctionExpMeta])(implicit specPath: String): Option[Map[FunctionMeta, FunctionExpMeta]]

  protected def mergeMaps(maps: Seq[Map[FunctionMeta, FunctionExpMeta]]): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    maps.foldLeft[Option[Map[FunctionMeta, FunctionExpMeta]]](Some(Map())) { (result, map) =>
      if(result.isEmpty) None
      else mergeMaps(result.get, map)
    }
  }

  // if both map the same meta var to different exp map exp are not equal
  private def mergeMaps(l: Map[FunctionMeta, FunctionExpMeta], r: Map[FunctionMeta, FunctionExpMeta]): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    val mapsCompatible = l.forall { case (meta, exp) =>
      if (r.contains(meta))
        r(meta) == exp
      else true
    }
    if (mapsCompatible)
      Some(l ++ r)
    else None
  }

}

// Can not solve recursive constraints (f(x,y) == x)
case class NonRecursiveConstraintSolver(ctorNames: Seq[String]) extends ConstraintSolver {
  def solve(constraints: Set[FunctionExpMeta])(implicit specPath: String): Option[Map[FunctionMeta, FunctionExpMeta]] =
    solve(constraints, Map())(specPath)

  protected def solve(constraints: Set[FunctionExpMeta], acc: Map[FunctionMeta, FunctionExpMeta])(implicit specPath: String): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    val metaVarCollection = new MetaVarCollection {}
    constraints.foreach { metaVarCollection.transFunctionExpMeta }
    val metaVars = metaVarCollection.metaVars
    val isComplete = metaVars.forall(acc.contains)
    if (isComplete)
      return Some(acc)
    val singleSolutions = constraints.map { getMapping }
    val partialSolution = mergeMaps(acc +: singleSolutions.toSeq)

    if (partialSolution.nonEmpty) {
      val subster = MetaVarSubstitution(partialSolution.get)
      val backsubstituion = constraints.map { subster.transFunctionExpMeta }
      val solved = backsubstituion.forall(DoesNotContainMetaVars.check)
      if (solved) partialSolution
      else {
        val solvableConstraints = getSolvableConstraints(backsubstituion)
        val nextSolution = mergeMaps(solvableConstraints.map(solveFunctionAppConstraint).toSeq)
        if (nextSolution.nonEmpty) {
          val mergedSolution = mergeMaps(Seq(partialSolution.get, nextSolution.get))
          if (mergedSolution == partialSolution) return None
          if (mergedSolution.nonEmpty) {
            solve(constraints, mergedSolution.get)
          }
          else None
        }
        else None
      }
    } else None
  }

  private def solveFunctionAppConstraint(constraint: FunctionExpMeta)(implicit specPath: String): Map[FunctionMeta, FunctionExpMeta] = constraint match {
    case FunctionExpEq(meta: FunctionMeta, exp: FunctionExpMeta) =>
      val result = ReflectionHelper.executeFunctionExp(exp)(specPath, Map())
      val registeredTermFunctionExpressionTranslator = RegisteredTermFunctionExpressionTranslator()
      val veritasResult = registeredTermFunctionExpressionTranslator.translateExp(ScalaMetaUtils.getTerm(exp.toString))
      Map(meta -> veritasResult)
    case FunctionExpEq(app: FunctionExpApp, exp: FunctionExpMeta) =>
      val result = ReflectionHelper.executeFunctionExp(exp)(specPath, Map())
      val registeredTermFunctionExpressionTranslator = RegisteredTermFunctionExpressionTranslator()
      val veritasResult = registeredTermFunctionExpressionTranslator.translateExp(ScalaMetaUtils.getTerm(result.toString))
      ConstraintUtil.removeCommonFunctionApplications(app, veritasResult).collect {
        case (key: FunctionMeta, value: FunctionExpMeta) => (key, value)
      }.toMap
    case _ => Map()
  }

  // Only allow ctor apps on the side with meta vars
  private def getSolvableConstraints(constraints: Set[FunctionExpMeta]): Set[FunctionExpMeta] =
    constraints.collect {
      case FunctionExpEq(meta: FunctionMeta, exp: FunctionExpMeta) if DoesNotContainMetaVars.check(exp) =>
        FunctionExpEq(meta, exp)
      case FunctionExpEq(exp: FunctionExpMeta, meta: FunctionMeta) if DoesNotContainMetaVars.check(exp) =>
        FunctionExpEq(meta, exp)
      case FunctionExpEq(lhs: FunctionExp, rhs: FunctionExp)
        if DoesNotContainMetaVars.check(lhs) && !DoesNotContainMetaVars.check(rhs) &&
          OnlyContainsCtorApplications.check(rhs)(ctorNames) =>
        FunctionExpEq(rhs, lhs)
      case FunctionExpEq(lhs: FunctionExp, rhs: FunctionExp)
        if !DoesNotContainMetaVars.check(lhs) && DoesNotContainMetaVars.check(rhs) &&
          OnlyContainsCtorApplications.check(lhs)(ctorNames) =>
        FunctionExpEq(lhs, rhs)
    }

  private def getMapping(constraint: FunctionExpMeta): Map[FunctionMeta, FunctionExpMeta] = constraint match {
    case FunctionExpEq(meta: FunctionMeta, rhs) =>
      if (isSolutionCandidate(rhs))
        Map(meta -> rhs)
      else Map()
    case FunctionExpEq(lhs, meta: FunctionMeta) =>
      if (isSolutionCandidate(lhs))
        Map(meta -> lhs)
      else Map()
    case _ => Map()
  }

  private def isSolutionCandidate(value: FunctionExpMeta): Boolean = {
    DoesNotContainMetaVars.check(value)
  }
}

trait OnlyContainsCtorApplications extends ModuleTransformation {
  def ctorNames: Seq[String]
  def containsOnlyCtorApplications: Boolean = _containsOnlyCtorApplications
  private[this] var _containsOnlyCtorApplications = true

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    withSuper(super.transFunctionExp(f)) {
      case FunctionExpApp(name, args) =>
        if (!ctorNames.contains(name)) {
          _containsOnlyCtorApplications = false
          FunctionExpApp(name, args)
        } else FunctionExpApp(name, args.map(transFunctionExpMeta))
    }
}

object OnlyContainsCtorApplications {
  def check(f: FunctionExpMeta)(names: Seq[String]): Boolean = {
    val checker = new OnlyContainsCtorApplications {
      val ctorNames: Seq[String] = names
    }
    checker.transFunctionExpMeta(f)
    checker.containsOnlyCtorApplications
  }
}

