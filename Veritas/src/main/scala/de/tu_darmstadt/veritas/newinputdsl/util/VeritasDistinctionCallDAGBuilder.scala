package de.tu_darmstadt.veritas.newinputdsl.util

import de.tu_darmstadt.veritas.backend.ast.{DataType, Module}
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VeritasDistinctionCallDAGBuilder(spec: Module) extends DistinctionCallDAGBuilder[FunctionDef, FunctionEq, FunctionExp , FunctionExpMeta, VeritasDistinctionCallDAG] {

  private val ctorNames = ListBuffer[String]()

  override def translate(funDef: FunctionDef)(dag: VeritasDistinctionCallDAG): DistinctionCallDAG[FunctionEq, FunctionExp, FunctionExpMeta] = {
    spec.defs.foreach {
      case DataType(_, _, ctors) =>
        ctorNames ++= ctors.map(_.name)
      case _ =>
    }
    super.translate(funDef)(dag)
  }

  protected def groupFunctionEquations(eqs: Seq[FunctionEq], positionToWatch: Int = 0): Seq[(Int, Set[FunctionEq])] = {
    val patternStrings = eqs.map { eq => (createStringFromPattern(eq.patterns(positionToWatch)), eq)}.sortBy(_._1)
    val grouped = mutable.Map[String, Set[FunctionEq]]()

    // group based on prefixes
    patternStrings.foreach { case (string, eq) =>
      val prefixes = substringByChar(string, '_')
      prefixes.foreach { prefix =>
        if (grouped.contains(prefix)) {
          grouped(prefix) = grouped(prefix) + eq
        } else {
          grouped(prefix) = Set(eq)
        }
      }
    }

    patternStrings.foreach { case (prefix, eq) =>
      if (!grouped.contains(prefix))
        grouped(prefix) = Set(eq)
    }

    // remove groupings which are more specifc but have the same elements
    val cleanedUpGroupings = grouped.filter { case (prefix, eqn) =>
      val moreGeneral = substringByChar(prefix, '_')
      moreGeneral.forall { pr2 =>
        val moreGeneralEqs = grouped(pr2)
        moreGeneralEqs != eqn
      }
    }
    cleanedUpGroupings.toSeq.sortBy(_._1).map(x => (x._1.split("_").length, x._2))
  }

  private def substringByChar(str: String, sign: Char): Seq[String] =
    str.zipWithIndex.filter { _ == sign}.map { case (_, index) => str.substring(0, index) }

  protected def createStringFromPattern(pattern: FunctionPattern): String = pattern match {
    case FunctionPatApp(name, args) =>
      name + "_" + args.map(createStringFromPattern).mkString("%")
    case FunctionPatVar(name) => name
  }

  override protected def getDistinctionByIfExpression(exp: FunctionExpMeta): Map[FunctionExp, FunctionExpMeta] = exp match {
    case FunctionExpIf(cond, thn, els) => Map() + (cond -> thn) + (FunctionExpNot(cond) -> els)
    case FunctionExpLet(_, namedExpr, in) =>
      getDistinctionByIfExpression(namedExpr) ++ getDistinctionByIfExpression(in)
    case _ => Map()
  }

  // TODO currently only supports sinlge funcapp
  override protected def getResultBindings(exp: FunctionExpMeta): Map[String, Set[String]] = exp match {
    case FunctionExpLet(name, FunctionExpApp(funcName, _), in) =>
      Map() + (name -> Set(funcName)) ++ getResultBindings(in)
    case _ => Map()
  }

  override protected def getNestedFunctionApplications(exp: FunctionExpMeta): Seq[Seq[String]] = exp match {
    case FunctionExpApp(name, args) =>
      val inner =
        if (!ctorNames.contains(name))
          Seq(Seq(name)) ++ args.flatMap(getFunctionApplication).map { Seq(name, _) }
        else Seq()
      inner ++ args.flatMap(getNestedFunctionApplications)
    case FunctionExpLet(_, named, in) => getNestedFunctionApplications(named) ++ getNestedFunctionApplications(in)
    case FunctionExpIf(cond, _, _)  => getNestedFunctionApplications(cond)
    case _ => Seq()
  }

  override protected def getFunctionApplication(exp: FunctionExpMeta): Option[String] = exp match {
    case FunctionExpApp(name, _) =>
      if (!ctorNames.contains(name)) Some(name)
      else None
    case _ => None
  }

  override protected def getEquationsOfDefintion(funDef: FunctionDef): Seq[FunctionEq] = funDef.eqn

  override protected def getVarRefWithinFunctionApp(exp: FunctionExpMeta): Map[String, Set[String]] = exp match {
    case FunctionExpApp(name, args) =>
      val refs =
        if (!ctorNames.contains(name))
          Map(name -> args.flatMap(getVarRefs).toSet)
        else Map()
      refs ++ args.flatMap(getVarRefWithinFunctionApp)
    case FunctionExpIf(cond, _, _) => getVarRefWithinFunctionApp(cond)
    case FunctionExpLet(_, namedExp, in) =>
      updateMap(getVarRefWithinFunctionApp(namedExp), getVarRefWithinFunctionApp(in))
    case FunctionExpNot(f) => getVarRefWithinFunctionApp(f)
    case FunctionExpEq(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpNeq(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpAnd(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpOr(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case FunctionExpBiImpl(lhs, rhs) =>
      updateMap(getVarRefWithinFunctionApp(lhs), getVarRefWithinFunctionApp(rhs))
    case _ => Map()
  }

  private def getVarRefs(exp:FunctionExpMeta): Option[String] = exp match {
    case FunctionExpVar(name) => Some(name)
    case _ => None
  }

  private def updateMap[K, V](prev: Map[K, Set[V]], now: Map[K, Set[V]]): Map[K, Set[V]] = {
    val result = mutable.Map() ++ prev
    now.foreach { case (key, vals) =>
      if (result.contains(key))
        result(key) = result(key) ++ vals
      else
        result(key) = vals
    }
    Map() ++ result
  }
}
