package de.tu_darmstadt.veritas.scalaspl.util


import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.FreshVariables
import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.DomainSpecificKnowledge
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VeritasAugmentedCallGraphBuilder(spec: Module) extends AugmentedCallGraphBuilder[DataType, TypingRule, FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta, VeritasAugmentedCallGraph] {

  private val ctorNames = ListBuffer[String]()
  private var constructors = Map[String, DataTypeConstructor]()
  private var funcSigs = Map[String, FunctionSig]()

  override def translate(funDef: FunctionDef, dsk: DomainSpecificKnowledge[DataType, FunctionDef, TypingRule])(dag: VeritasAugmentedCallGraph): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] = {
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
    super.translate(funDef, dsk)(dag)
  }

  override protected def getDistinctionByIfExpression(exp: FunctionExpMeta): Map[FunctionExp, FunctionExpMeta] = exp match {
    case FunctionExpIf(cond, thn, els) => Map() + (cond -> thn) + (FunctionExpNot(cond) -> els)
    case FunctionExpLet(_, namedExpr, in) =>
      getDistinctionByIfExpression(namedExpr) ++ getDistinctionByIfExpression(in)
    case _ => Map()
  }

  // TODO currently only supports sinlge funcapp - there might be a lot of cases missing here, some bindings might not be detected correctly!
  override protected def getResultBindings(exp: FunctionExpMeta, nestinglevel: Int): Map[String, Set[(String, Int)]] = exp match {
    case FunctionExpLet(name, FunctionExpApp(funcName, _), in) =>
      Map() + (name -> Set((funcName, nestinglevel))) ++ getResultBindings(in, nestinglevel)
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
  // create a common argument expression for the given position, together with the total argument position list in which the
  // expressions in the group will be distinguished further (if possible)
  // e.g. a group with the single entry (3, Succ(t1)) creates (None, Succ(t1))
  // a group with three entries [(0, Ifelse(True(), t2, t3)), (1, Ifelse(False(), t2, t3)), (2, Ifelse(t1, t2, t3))]
  // creates (Some([0]), Ifelse(t, t2, t3)) where t is a generated fresh variable name
  override protected def makeArgExpWithDistPos(eqs: Seq[(Int, FunctionEq)], argexp_list: Seq[FunctionExpMeta], distarg_pos: Seq[Int], dag: VeritasAugmentedCallGraph): (Option[Seq[Int]], FunctionExpMeta) = {
    //common argument expression should be an equation; retrieve variable for lhs from parent_argexp
    //for groups with just one element, we can immediately return the result
    if (eqs.length == 1) {
      val pat = getFunctionPatternAtPos(eqs.head._2, distarg_pos)
      val lhvarexpr = dag.getVarExpAtDistarg_pos(argexp_list, distarg_pos)
      (None, FunctionExpEq(lhvarexpr, makeFunctionExpFromPat(pat.get)))
    } else {
      var considered_distarg_pos = distarg_pos //starting point for positions to still consider
      var next_dist_pos: Option[Int] = None // next position for distinguishing within the current argument list (to be discovered)
      var total_dist_pos: Option[Seq[Int]] = None //total position list for the result (to be discovered)
      var common_exp: FunctionExpMeta = argexp_list.last //without any refinement of the grouping, the common expression of the group stays the one from the parent
      //if the current distarg_pos list does not reveal any position for distinguishing, we may have to backtrack, hence the while loop
      //we have to terminate if we either find a valid position for distinguishing the arguments or if we run out of valid positions
      while (next_dist_pos.isEmpty && considered_distarg_pos.nonEmpty) {
        //we assume a valid position list (considered_distarg_pos) at this point
        val patlist: Seq[(Int, FunctionPattern)] = for ((i, eq) <- eqs) yield {
          (i, getFunctionPatternAtPos(eq, considered_distarg_pos).get)
        }

        val groups = groupFunctionPatterns(patlist)

        if (groups.length == 1) {
          // if there is only one group, it has to be a group with the same function application

          val pats = patlist map (_._2)

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
              case Some(p) => Some(considered_distarg_pos :+ p)
              case None => None
            }

            //create a common argument expression (equation) for the group
            val lhvarexpr = dag.getVarExpAtDistarg_pos(argexp_list, considered_distarg_pos)

            val lhvarmv = lhvarexpr match {
              case FunctionMeta(mv@MetaVar(_)) => mv
            }

            val rhs = if (constructors.isDefinedAt(funname)) {
              val varnames = FreshVariables.freshMetaVars(Set(lhvarmv), constructors(funname).in)
              FunctionExpApp(funname, varnames map (v => FunctionMeta(v)))
            }
            else {
              val varnames = FreshVariables.freshMetaVars(Set(lhvarmv), funcSigs(funname).in)
              FunctionExpApp(funname, varnames map (v => FunctionMeta(v)))
            }


            common_exp = FunctionExpEq(lhvarexpr, rhs)
          } else {
            sys.error(s"Could not detect a common pattern for group $eqs at argument position $considered_distarg_pos")
          }

          //compute the next valid argument position for checking if no position for distinguishing has been found yet
          if (next_dist_pos.isEmpty)
            considered_distarg_pos = retrieveNextPatternPos(eqs, considered_distarg_pos)
        } else {
          //if there is more than one group in the argument list, then the current position is the one by which we have to distinguish next
          next_dist_pos = Some(considered_distarg_pos.last)
          total_dist_pos = Some(considered_distarg_pos)

          //correct common expression was computed in a previous iteration
        }
      }
      (total_dist_pos, common_exp)
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
