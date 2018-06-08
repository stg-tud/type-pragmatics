package de.tu_darmstadt.veritas.scalaspl.util

import scala.collection.mutable.ListBuffer

trait AugmentedCallGraphBuilder[FunDef, Eq, Criteria, Exp, Graph <: AugmentedCallGraph[Eq, Criteria, Exp]] {

  def translate(funDef: FunDef)(dag: Graph): AugmentedCallGraph[Eq, Criteria, Exp] = {
    val groupedEquations = groupFunctionEquations(getEquationsOfDefintion(funDef))
    val maxLevel = groupedEquations.map(_._1).max
    val proccesedEquations: ListBuffer[(Int, dag.StructuralDistinction)] = ListBuffer()

    // add root with all function equations
    val root = dag.StructuralDistinction(getEquationsOfDefintion(funDef).toSet)
    dag.addRoot(root)
    proccesedEquations += 1 -> root
    // Idea: group is parent of other group if it is a superset of it and has a lower lvl
    def buildChildrenBasedOnPattern(level: Int): Unit = {
      val currentLevelEquations = groupedEquations.filter(_._1 == level)
      currentLevelEquations.foreach { case (lvl, eqs) =>
        val parentCandidates = proccesedEquations.filter(x => eqs.forall(x._2.eqs.contains))
        // direct parent is the smallest superset of the eqs
        val parent = parentCandidates.minBy(_._2.eqs.size)
        val child = dag.StructuralDistinction(eqs)
        dag.addChild(parent._2, child)
        proccesedEquations += lvl -> child
      }
      if (level < maxLevel)
        buildChildrenBasedOnPattern(level + 1)
    }
    buildChildrenBasedOnPattern(2)

    val leaves = dag.leaves
    // every leave at this stage has to be a structural leave
    val structuralLeaves = leaves.map(_.asInstanceOf[dag.StructuralDistinction])

    def buildChildrenBasedOnFunctionExp(node: dag.Node, foundBindings: Map[String, Set[String]]): Unit = {
      val exp = dag.getExpression(node)
      val nestedFunctionApps = getNestedFunctionApplications(exp)
      nestedFunctionApps.foreach { funcNames =>
        val outerNode = dag.FunctionCall(funcNames.head)
        dag.addChild(outerNode, node)
        if (funcNames.lengthCompare(2) == 0) {
          val innerNode = dag.FunctionCall(funcNames(1))
          dag.addChild(innerNode, outerNode)
        }
      }

      // maps from function name to passed varrefs
      val varRefdByFunction: Map[String, Set[String]] = getVarRefWithinFunctionApp(exp)
      // maps from var name to functions it used to create the binding
      val bindings: Map[String, Set[String]] = foundBindings ++ getResultBindings(exp)
      varRefdByFunction.foreach { case (funName, refNames) =>
        val funCall = dag.FunctionCall(funName)
        // because function was called in within a let or a the cond of if we link node to funCall
        dag.addChild(funCall, node)

        bindings.foreach { case (bindingName, funcApps) =>
          // binding was used in a funApp
          if (refNames.contains(bindingName)) {
            funcApps.foreach { funcName =>
              val parent = dag.FunctionCall(funcName)
              dag.addChild(parent, funCall)
            }
          }
        }
      }
      // create children based on if condition
      val conditionalBranches = getDistinctionByIfExpression(dag.getExpression(node))
      conditionalBranches.foreach { case (condition, branch) =>
        val branchNode = dag.BooleanDistinction(condition, branch)
        dag.addChild(node, branchNode)
        buildChildrenBasedOnFunctionExp(branchNode, bindings)
      }
    }

    //
    structuralLeaves.foreach { leave =>
      buildChildrenBasedOnFunctionExp(leave, Map())
    }
    dag
  }

  protected def getEquationsOfDefintion(funDef: FunDef): Seq[Eq]

  protected def groupFunctionEquations(eqs: Seq[Eq], positionToWatch: Int = 0): Seq[(Int, Set[Eq])]

  protected def getDistinctionByIfExpression(exp: Exp): Map[Criteria, Exp]

  protected def getFunctionApplication(exp: Exp): Option[String]
  // list of lists where inner list has one element or two elements
  // first element is outer function application, second is a possible nested function application
  protected def getNestedFunctionApplications(exp: Exp): Seq[Seq[String]]
  protected def getVarRefWithinFunctionApp(exp: Exp): Map[String, Set[String]]
  protected def getResultBindings(exp: Exp): Map[String, Set[String]]
}
