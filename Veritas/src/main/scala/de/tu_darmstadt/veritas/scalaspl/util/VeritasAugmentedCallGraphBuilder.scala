package de.tu_darmstadt.veritas.scalaspl.util

import java.lang.IndexOutOfBoundsException

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.FreshVariables
import de.tu_darmstadt.veritas.backend.ast.{DataType, MetaVar, Module}
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VeritasAugmentedCallGraphBuilder(spec: Module) extends AugmentedCallGraphBuilder[FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta, VeritasAugmentedCallGraph] {

  private val ctorNames = ListBuffer[String]()

  override def translate(funDef: FunctionDef, distargpos: Int = 0)(dag: VeritasAugmentedCallGraph): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] = {
    spec.defs.foreach {
      case DataType(_, _, ctors) =>
        ctorNames ++= ctors.map(_.name)
      case _ =>
    }
    super.translate(funDef)(dag)
  }

  //  protected def groupFunctionEquations(eqs: Seq[FunctionEq], positionToWatch: Int = 0): Seq[(Int, Seq[FunctionEq])] = {
  //    val patternStrings = eqs.map { eq => (createStringFromPattern(eq.patterns(positionToWatch)), eq)}.sortBy(_._1)
  //    val grouped = mutable.Map[String, Seq[FunctionEq]]()
  //
  //    // group based on prefixes
  //    patternStrings.foreach { case (string, eq) =>
  //      val prefixes = substringByChar(string, '_')
  //      prefixes.foreach { prefix =>
  //        if (grouped.contains(prefix)) {
  //          grouped(prefix) = grouped(prefix) :+ eq
  //        } else {
  //          grouped(prefix) = Seq(eq)
  //        }
  //      }
  //    }
  //
  //    patternStrings.foreach { case (prefix, eq) =>
  //      if (!grouped.contains(prefix))
  //        grouped(prefix) = Seq(eq)
  //    }
  //
  //    // remove groupings which are more specifc but have the same elements
  //    val cleanedUpGroupings = grouped.filter { case (prefix, eqn) =>
  //      val moreGeneral = substringByChar(prefix, '_')
  //      moreGeneral.forall { pr2 =>
  //        val moreGeneralEqs = grouped(pr2)
  //        moreGeneralEqs != eqn
  //      }
  //    }
  //    cleanedUpGroupings.toSeq.sortBy(_._1).map(x => (x._1.split("_").length, x._2))
  //  }

  private def substringByChar(str: String, sign: Char): Seq[String] =
    str.zipWithIndex.filter {
      _ == sign
    }.map { case (_, index) => str.substring(0, index) }

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
          Seq(Seq(name)) ++ args.flatMap(getFunctionApplication).map {
            Seq(name, _)
          }
        else Seq()
      inner ++ args.flatMap(getNestedFunctionApplications)
    case FunctionExpLet(_, named, in) => getNestedFunctionApplications(named) ++ getNestedFunctionApplications(in)
    case FunctionExpIf(cond, _, _) => getNestedFunctionApplications(cond)
    case _ => Seq()
  }

  override protected def getFunctionApplication(exp: FunctionExpMeta): Option[String] = exp match {
    case FunctionExpApp(name, _) =>
      if (!ctorNames.contains(name)) Some(name)
      else None
    case _ => None
  }

  override protected def makeGenericFunctionCall(fundef: FunctionDef): FunctionExpMeta =
    fundef match {
      case FunctionDef(FunctionSig(name, in, _), _) => {
        val varnames = FreshVariables.freshMetaVars(Set(), in)
        FunctionExpApp(name, varnames map (v => FunctionMeta(v)))
      }
    }

  // given a list of equations and an argument position,
  // create a common argument expression for the given position, together with the single argument position in which the
  // expressions in the group will be distinguished further (if possible)
  // e.g. a group with the single entry (3, Succ(t1)) creates (None, Succ(t1))
  // a group with three entries [(0, Ifelse(True(), t2, t3)), (1, Ifelse(False(), t2, t3)), (2, Ifelse(t1, t2, t3))]
  // creates (Some(0), Ifelse(t, t2, t3)) where t is a generated fresh variable name
  override protected def makeArgExpWithDistPos(eqs: Seq[(Int, FunctionEq)], distarg_pos: Seq[Int]): (Option[Int], FunctionExpMeta) = {
    //for groups with just one element, we can immediately return the result
    if (eqs.length == 1) {
      val pat = getFunctionPatternAtPos(eqs.head._2, distarg_pos)
      (None, makeFunctionExpFromPat(pat))
    } else {
      val pats = for (eq <- eqs) yield getFunctionPatternAtPos(eq._2, distarg_pos)
      //the function patterns within a group at the position designated by distarg_pos all have to be FunctionPatApps with the same name!
      val funnames = (for (p <- pats) yield p match {
        case FunctionPatApp(functionName, _) => functionName
      }).distinct
      if (funnames.length == 1) {
        //expected case for wellformed group
        val next_dist_pos: Option[Int] = ???
        val common_exp: FunctionExpMeta = ???
        (next_dist_pos, common_exp)
      } else {
        sys.error(s"Could not detect a common pattern for group $eqs")
      }

    }
  }

  private def makeFunctionExpFromPat(pattern: FunctionPattern): FunctionExpMeta =
    pattern match {
      case FunctionPatVar(name) => FunctionMeta(MetaVar(name))
      case FunctionPatApp(name, args) => FunctionExpApp(name, args map (p => makeFunctionExpFromPat(p)))
    }

  private def getFunctionPatternAtPos(eq: FunctionEq, distarg_pos: Seq[Int]): FunctionPattern = {
    eq match {
      case FunctionEq(functionName, patterns, _) =>
        var currpatlist = patterns
        var currposlist = distarg_pos
        while (currposlist.nonEmpty) {
          try {
            currpatlist = patterns(currposlist.head) match {
              case v@FunctionPatVar(_) => Seq(v)
              case FunctionPatApp(_, args) => args
            }
            currposlist = currposlist.tail
          } catch {
            case e: IndexOutOfBoundsException => sys.error(s"The position indicated by $distarg_pos was not reachable in function $functionName")
          }
        }
        currpatlist.head
    }
  }

  override protected def makeGroupsForPos(eqs_to_group: Seq[(Int, FunctionEq)], poslist: Seq[Int]): Seq[Seq[(Int, FunctionEq)]] = {
    //obtain list of patterns at the position indicated by poslist; keep the index of the function equation with the selected pattern
    val patlist: Seq[(Int, FunctionPattern)] = for ((i, eq) <- eqs_to_group) yield {
          (i, getFunctionPatternAtPos(eq, poslist))
      }

    //now group the patterns
    //first grouping step: distinguish FunctionPatVar and FunctionPatApp
    val (patvargroup, patappgroup) = patlist.partition(ifp =>
      ifp._2 match {
        case FunctionPatVar(_) => true
        case _ => false
      })

    //second grouping step: group FunctionPatApp by name of function
    val groupedpatapps: Seq[Seq[(Int, FunctionPattern)]] = patappgroup.groupBy(ifp =>
      ifp._2 match {
        case FunctionPatApp(name, _) => name
      }).values.toSeq

    //third step: concatenate; each FunctionPatVar gets its own group
    val patterngrouping: Seq[Seq[(Int, FunctionPattern)]] = groupedpatapps ++ (patvargroup map (Seq(_)))

    //fourth step: make sure the groups are sorted within themselves by function eq index
    // and sort the groups by first function eq index within group
    val sortedpatterngroups: Seq[Seq[(Int, FunctionPattern)]] = for (group <- patterngrouping) yield group.sortBy(_._1)
    val finalpatternsorting: Seq[Seq[(Int, FunctionPattern)]] = sortedpatterngroups.sortBy(_.head._1)

    //now transfer the grouping of the patterns back to the original function equations (creates correctly sorted groups)
    for (ifp <- finalpatternsorting) yield
      for ((i, fp) <- ifp) yield
        eqs_to_group.find(ifp => ifp._1 == i).get

  }

  override protected def getEquationsOfDefinition(funDef: FunctionDef): Seq[FunctionEq] = funDef.eqn

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

  private def getVarRefs(exp: FunctionExpMeta): Option[String] = exp match {
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
