package de.tu_darmstadt.veritas.scalaspl.util

import java.lang.IndexOutOfBoundsException

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.FreshVariables
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VeritasAugmentedCallGraphBuilder(spec: Module) extends AugmentedCallGraphBuilder[FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta, VeritasAugmentedCallGraph] {

  private val ctorNames = ListBuffer[String]()
  private var constructors = Map[String, DataTypeConstructor]()
  private var funcSigs = Map[String, FunctionSig]()

  override def translate(funDef: FunctionDef, distargpos: Int = 0)(dag: VeritasAugmentedCallGraph): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] = {
    spec.defs.foreach {
      case DataType(_, _, ctors) => {
        ctorNames ++= ctors.map(_.name)
        for (ct <- ctors) {
          constructors += (ct.name -> ct)
        }
      }
      case Functions(Seq(FunctionDef(sig@FunctionSig(name, _, _), _))) => funcSigs += (name -> sig)
      case _ =>
    }
    super.translate(funDef)(dag)
  }

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

  //internal helper function
  private def getVarExpAtDistarg_pos(arg: FunctionExpMeta, distarg_pos: Seq[Int]): FunctionMeta = {

    def retrieveArgList(exp: FunctionExpMeta): Seq[FunctionExpMeta] =
      exp match {
        case FunctionExpEq(_, FunctionExpApp(_, args)) => args
        case FunctionExpApp(_, args) => args
        case a => Seq(a)
      }

    var currarglist = retrieveArgList(arg)
    var currdistpos = distarg_pos
    while (currdistpos.nonEmpty && currarglist.length > 1) {
      currarglist = retrieveArgList(currarglist(currdistpos.head))
      currdistpos = currdistpos.tail
    }

    currarglist.head match {
      case fm@FunctionMeta(MetaVar(_)) => fm
      case _ => sys.error(s"left-hand side variable name could not be found - ill-constructed parent argument expression? $arg")
    }

  }

  //construct groups of pattern arguments
  //predicate for determining whether an argument position is a next candidate for distinguishing a group
  private def distPosYesNo(arglist: Seq[FunctionPattern]): Boolean = {
    //first step for determining a distarg pos: Are there constructor calls AND variable calls within the group?
    val (patvargroup, patappgroup) = arglist.partition {
      case FunctionPatVar(_) => true
      case _ => false
    }

    if (patvargroup.nonEmpty)
      patappgroup.nonEmpty //two cases where we can immediately return the result:
    // 1) patappgroup is empty, so variables only in group: no position for distinguishing, return false
    // 2) patappgroup is not empty, so variables AND constructors are present: definitely a position for distinguishing, return true
    else if (patappgroup.nonEmpty) {
      // we need to investigate the patappgroups further
      //second step for determining a distarg pos: group FunctionPatApp by name of function
      val groupedpatapps = patappgroup.groupBy {
        case FunctionPatApp(name, _) => name
      }.values.toSeq

      if (groupedpatapps.length > 1)
        true //is there more than one group? -> if yes, we definitely have to distinguish here
      else {
        // otherwise we have to recursively investigate the arguments of the group
        // we might generate some superfluous structural case distinctions by returning the outer position as distarg
        // already if there is some inner distarg_pos - is this a problem?
        val singlegroup = groupedpatapps.head
        //group the single arguments within the FunctionPatApp with the same name by position
        lazy val listofargs = for (p <- singlegroup) yield p match {
          case FunctionPatApp(_, args) => args
        }

        lazy val arggroups = for (i <- listofargs.head.indices) yield listofargs map (la => la(i))

        arggroups.exists(distPosYesNo)
      }
    }
    else
    //both groups empty, should not happen, return false just in case
      false
  }

  //calculate the next valid pattern position
  private def retrieveNextPatternPos(eqs: Seq[(Int, FunctionEq)], distarg_pos: Seq[Int]): Seq[Int] = {
    //try next position by augmenting last element of list
    if (distarg_pos.isEmpty)
      Seq() //we did maximum backtracking and hence return the empty list to indicate that there is no next pattern position
    else {
      val nexttry = distarg_pos.init :+ (distarg_pos.last + 1)
      val pats = for (eq <- eqs) yield getFunctionPatternAtPos(eq._2, nexttry)
      if (pats.contains(None))
        //backtrack to previous position (will be augmented by one in the recursive call!)
        retrieveNextPatternPos(eqs, distarg_pos.init)
      else
        nexttry
    }
  }


  // given a list of equations and an argument position,
  // create a common argument expression for the given position, together with the single argument position in which the
  // expressions in the group will be distinguished further (if possible)
  // e.g. a group with the single entry (3, Succ(t1)) creates (None, poslist, Succ(t1))
  // a group with three entries [(0, Ifelse(True(), t2, t3)), (1, Ifelse(False(), t2, t3)), (2, Ifelse(t1, t2, t3))]
  // creates (Some(0), poslist, Ifelse(t, t2, t3)) where t is a generated fresh variable name
  // and where poslist is the total new position indication of the distinguishing argument position within the given function equation
  // (may change due to backtracking)
  override protected def makeArgExpWithDistPos(eqs: Seq[(Int, FunctionEq)], parent_argexp: FunctionExpMeta, distarg_pos: Seq[Int]): (Option[Int], Seq[Int], FunctionExpMeta) = {
    //common argument expression should be an equation; retrieve variable for lhs from parent_argexp
    //for groups with just one element, we can immediately return the result
    if (eqs.length == 1) {
      val pat = getFunctionPatternAtPos(eqs.head._2, distarg_pos)
      val lhvarname = getVarExpAtDistarg_pos(parent_argexp, distarg_pos)
      (None, distarg_pos, FunctionExpEq(lhvarname, makeFunctionExpFromPat(pat.get)))
    } else {
      var considered_distarg_pos = distarg_pos
      var next_dist_pos: Option[Int] = None
      var total_dist_pos: Seq[Int] = Seq()
      var common_exp: FunctionExpMeta = FunctionExpApp("PLACEHOLDER", Seq())
      //if the current distarg_pos list does not reveal any position for distinguishing, we may have to backtrack, hence the while loop
      while (considered_distarg_pos.nonEmpty || next_dist_pos.isEmpty) {
        //we assume a valid position list (considered_distarg_pos) at this point
        val patlist: Seq[(Int, FunctionPattern)] = for ((i, eq) <- eqs) yield {
          (i, getFunctionPatternAtPos(eq, considered_distarg_pos).get)
        }

        val groups = groupFunctionPatterns(patlist)
        //TODO: analyze groups correctly here

        val pats = (for (eq <- eqs) yield getFunctionPatternAtPos(eq._2, considered_distarg_pos)).flatten

        // first grouping step for pats: distinguish

        //the function patterns within a group at the position designated by distarg_pos all have to be FunctionPatApps with the same name!
        val funnames = (for (p <- pats) yield p match {
          case FunctionPatApp(functionName, _) => functionName
        }).distinct
        if (funnames.length == 1) {
          //expected case for wellformed group consisting only of application patterns
          val funname = funnames.head

          //group the single arguments within the FunctionPatApp with the same name by position
          val listofargs = for (p <- pats) yield p match {
            case FunctionPatApp(_, args) => args
          }

          val arggroups = for (i <- listofargs.head.indices) yield listofargs map (la => la(i))

          val ind_cand = arggroups.indexWhere(distPosYesNo)
          next_dist_pos = if (ind_cand >= 0) Some(ind_cand) else None
          total_dist_pos = next_dist_pos match {
            case Some(p) => considered_distarg_pos :+ p
            case None => considered_distarg_pos
          }

          //create a common argument expression (equation) for the group
          val rhs = if (constructors.isDefinedAt(funname)) {
            val varnames = FreshVariables.freshMetaVars(Set(), constructors(funname).in)
            FunctionExpApp(funname, varnames map (v => FunctionMeta(v)))
          }
          else {
            val varnames = FreshVariables.freshMetaVars(Set(), funcSigs(funname).in)
            FunctionExpApp(funname, varnames map (v => FunctionMeta(v)))
          }

          val lhvarname = getVarExpAtDistarg_pos(parent_argexp, considered_distarg_pos)
          common_exp = FunctionExpEq(lhvarname, rhs)
        } else {
          sys.error(s"Could not detect a common pattern for group $eqs at argument position $considered_distarg_pos")
        }
        considered_distarg_pos = retrieveNextPatternPos(eqs, considered_distarg_pos)
      }
      (next_dist_pos, total_dist_pos, common_exp)
    }
  }

  private def makeFunctionExpFromPat(pattern: FunctionPattern): FunctionExpMeta

  =
    pattern match {
      case FunctionPatVar(name) => FunctionMeta(MetaVar(name))
      case FunctionPatApp(name, args) => FunctionExpApp(name, args map (p => makeFunctionExpFromPat(p)))
    }

  private def getFunctionPatternAtPos(eq: FunctionEq, distarg_pos: Seq[Int]): Option[FunctionPattern] = {
    eq match {
      case FunctionEq(functionName, patterns, _) =>
        var currpatlist = patterns
        var currposlist = distarg_pos
        var resultpat: FunctionPattern = FunctionPatVar("UNINITIALIZED")
        while (currposlist.nonEmpty && currpatlist.nonEmpty) {
          try {
            resultpat = currpatlist(currposlist.head)
            currpatlist = resultpat match {
              case v@FunctionPatVar(_) => Seq()
              case app@FunctionPatApp(_, args) => args
            }
            currposlist = currposlist.tail
          } catch {
            case e: IndexOutOfBoundsException => None
          }
        }
        Some(resultpat)
    }
  }


  override protected def makeGroupsForPos(eqs_to_group: Seq[(Int, FunctionEq)], poslist: Seq[Int]): Seq[Seq[(Int, FunctionEq)]] = {
    //obtain list of patterns at the position indicated by poslist; keep the index of the function equation with the selected pattern
    val patlist: Seq[(Int, FunctionPattern)] = for ((i, eq) <- eqs_to_group) yield {
      (i, getFunctionPatternAtPos(eq, poslist).get)
    }

    //first group the patterns
    val finalpatternsorting = groupFunctionPatterns(patlist)

    //now transfer the grouping of the patterns back to the original function equations (creates correctly sorted groups)
    for (ifp <- finalpatternsorting) yield
      for ((i, fp) <- ifp) yield
        eqs_to_group.find(ifp => ifp._1 == i).get

  }

  //calculate groups from a list of function patterns together with the number of the function equation from which the pattern was extracted
  //will keep the individual equation number with the resulting grouped patterns
  private def groupFunctionPatterns(patlist: Seq[(Int, FunctionPattern)]): Seq[Seq[(Int, FunctionPattern)]] = {
    //first grouping step: distinguish FunctionPatVar and FunctionPatApp
    val (patvargroup, patappgroup) = patlist.partition(ifp =>
      ifp._2 match {
        case FunctionPatVar(_) => true
        case _ => false
      })

    //second grouping step: group FunctionPatApp by name of function
    val groupedpatapps = patappgroup.groupBy(ifp =>
      ifp._2 match {
        case FunctionPatApp(name, _) => name
      }).values

    //third step: concatenate; each FunctionPatVar gets its own group
    val patterngrouping = groupedpatapps.toSeq ++ (patvargroup map (Seq(_)))

    //fourth step: make sure the groups are sorted within themselves by function eq index
    // and sort the groups by first function eq index within group
    val sortedpatterngroups = for (group <- patterngrouping) yield group.sortBy(_._1)
    val finalpatternsorting = sortedpatterngroups.sortBy(_.head._1)
    finalpatternsorting
  }

  override protected def getEquationsOfDefinition(funDef: FunctionDef): Seq[FunctionEq]

  = funDef.eqn

  override protected def getVarRefWithinFunctionApp(exp: FunctionExpMeta): Map[String, Set[String]]

  = exp match {
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

  private def getVarRefs(exp: FunctionExpMeta): Option[String]

  = exp match {
    case FunctionExpVar(name) => Some(name)
    case _ => None
  }

  private def updateMap[K, V](prev: Map[K, Set[V]], now: Map[K, Set[V]]): Map[K, Set[V]]

  = {
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
