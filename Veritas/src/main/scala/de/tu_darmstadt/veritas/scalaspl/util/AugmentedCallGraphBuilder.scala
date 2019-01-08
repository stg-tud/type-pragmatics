package de.tu_darmstadt.veritas.scalaspl.util

import scala.collection.mutable.ListBuffer

trait AugmentedCallGraphBuilder[FunDef, Eq, Criteria, Exp, Graph <: AugmentedCallGraph[Eq, Criteria, Exp]] {

  //distargpos: argument position for first distinction; default value 0, i.e. the first argument of the function decides the grouping of equations
  // we do not yet support nested arguments here! position level refers to top-level argument list!
  //
  def translate(funDef: FunDef, distargpos: Int = 0)(dag: Graph): AugmentedCallGraph[Eq, Criteria, Exp] = {
    val funeqs = getEquationsOfDefinition(funDef)
    val root_argexp = makeGenericFunctionCall(funDef)
    val numbered_eqs = for (eq <- funeqs) yield (funeqs.indexOf(eq), eq)

    // add root with all function equations
    val root = dag.StructuralDistinction(Some(Seq(distargpos)), root_argexp, numbered_eqs)
    dag.addRoot(root)

    //inner recursive function: add structural distinction node level to graph
    def refineStructuralDistinctionLevel(parent: dag.StructuralDistinction, argexp_list: Seq[Exp], distargpos_list: Seq[Int], level: Int): Unit = {
      val eqs_to_group = parent.numbered_eqs
      val groups_for_next_level = makeGroupsForPos(eqs_to_group, distargpos_list)
      for (g <- groups_for_next_level) {
        val (new_distargpos_list, argexp_for_group) = makeArgExpWithDistPos(g, argexp_list, distargpos_list)
        val child = dag.StructuralDistinction(new_distargpos_list, argexp_for_group, g)
        dag.addChild(parent, child)
        if (g.length > 1) { //only refine further if the group still contains more than one function equation
          refineStructuralDistinctionLevel(child, argexp_list :+ argexp_for_group, new_distargpos_list.get, level + 1)
        }
      }
    }

    refineStructuralDistinctionLevel(root, Seq(root_argexp), Seq(distargpos), 2)

    val leaves = dag.leaves
    // every leaf at this stage has to be a structural leaf
    val structuralLeaves = leaves.map(_.asInstanceOf[dag.StructuralDistinction])

    def buildChildrenBasedOnFunctionExp(node: dag.Node, foundBindings: Map[String, Set[String]], nestinglevel: Int): Unit = {
      val exp = dag.getExpression(node)
      val eqnum_in_node = node match {
        case dag.StructuralDistinction(_, _, eqs) => eqs.head._1
        case dag.BooleanDistinction(num, _, _, _) => num
        case dag.FunctionCall(num, _, _) => num
      }
      val nestedFunctionApps = getNestedFunctionApplications(exp)
      nestedFunctionApps.foreach { funcNames =>
        val outerNode = dag.FunctionCall(eqnum_in_node, nestinglevel, funcNames.head)
        dag.addChild(outerNode, node)
        if (funcNames.lengthCompare(2) == 0) {
          val innerNode = dag.FunctionCall(eqnum_in_node, nestinglevel, funcNames(1))
          dag.addChild(innerNode, outerNode)
        }
      }

      // maps from function name to passed varrefs
      val varRefdByFunction: Map[String, Set[String]] = getVarRefWithinFunctionApp(exp)
      // maps from var name to functions it used to create the binding
      val bindings: Map[String, Set[String]] = foundBindings ++ getResultBindings(exp)
      varRefdByFunction.foreach { case (funName, refNames) =>
        val funCall = dag.FunctionCall(eqnum_in_node, nestinglevel, funName)
        // because function was called in within a let or a the cond of if we link node to funCall
        dag.addChild(funCall, node)

        bindings.foreach { case (bindingName, funcApps) =>
          // binding was used in a funApp
          if (refNames.contains(bindingName)) {
            funcApps.foreach { funcName =>
              val parent = dag.FunctionCall(eqnum_in_node, nestinglevel, funcName)
              dag.addChild(parent, funCall)
            }
          }
        }
      }
      // create children based on if condition
      val conditionalBranches = getDistinctionByIfExpression(dag.getExpression(node))
      conditionalBranches.foreach { case (condition, branch) =>
        val branchNode = dag.BooleanDistinction(eqnum_in_node, nestinglevel, condition, branch)
        dag.addChild(node, branchNode)
        buildChildrenBasedOnFunctionExp(branchNode, bindings, nestinglevel + 1)
      }
    }

    //
    structuralLeaves.foreach { leave =>
      buildChildrenBasedOnFunctionExp(leave, Map(), 0)
    }
    dag
  }

  protected def makeGenericFunctionCall(fundef: FunDef): Exp

  // given a list of equations and an argument position,
  // create a common argument expression for the given position, together with the total argument position list in which the
  // expressions in the group will be distinguished further (if possible)
  // e.g. a group with the single entry (3, Succ(t1)) creates (None, Succ(t1))
  // a group with three entries [(0, Ifelse(True(), t2, t3)), (1, Ifelse(False(), t2, t3)), (2, Ifelse(t1, t2, t3))]
  // creates (Some([0]), Ifelse(t, t2, t3)) where t is a generated fresh variable name
  protected def makeArgExpWithDistPos(eqs: Seq[(Int, Eq)], argexp_list: Seq[Exp], distarg_pos: Seq[Int]): (Option[Seq[Int]], Exp)

  protected def getEquationsOfDefinition(funDef: FunDef): Seq[Eq]

  //given a list of numbered equations, create the next grouping level for the argument position indicated by poslist
  //i.e. poslist = [0, 2] would indicate the third argument within a function/constructor call at the first argument of the original function
  protected def makeGroupsForPos(eqs_to_group: Seq[(Int, Eq)], poslist: Seq[Int]): Seq[Seq[(Int, Eq)]]

  //protected def groupFunctionEquations(eqs: Seq[(Int, Eq)]): Map[Int, Seq[(Int, Eq)]]

  protected def getDistinctionByIfExpression(exp: Exp): Map[Criteria, Exp]

  protected def getFunctionApplication(exp: Exp): Option[String]
  // list of lists where inner list has one element or two elements
  // first element is outer function application, second is a possible nested function application
  protected def getNestedFunctionApplications(exp: Exp): Seq[Seq[String]]
  protected def getVarRefWithinFunctionApp(exp: Exp): Map[String, Set[String]]
  protected def getResultBindings(exp: Exp): Map[String, Set[String]]
}
