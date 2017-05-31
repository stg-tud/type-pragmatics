package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import z3.scala.{Z3Context, Z3Model, Z3Solver}

/**
  * abstract template for calling Z3 with constraints
  */
abstract class SolveWithZ3[Spec, Goal] {

  type Var // Variable type

  val z3config: Map[String, Any] = Map("MODEL" -> true, "timeout" -> 60000)

  val context: Z3Context = new Z3Context(z3config.iterator.toList: _*)
  val solver: Z3Solver = context.mkSolver()

  def makeSolutionVariables(goal: Goal): Array[Var]

  def makeAndAddConstraints(spec: Spec, goal: Goal, vars: Array[Var]): Unit

  def parseResult(goal: Goal)(m: Z3Model, vars: Array[Var]): Array[String]

  def solveAndGetResult(spec: Spec, goal: Goal, vars: Array[Var]): Array[String] = {
    val vars = makeSolutionVariables(goal)
    makeAndAddConstraints(spec, goal, vars)

    solver.check() match {
      case None => Array("Z3 failed. The reason is: " + solver.getReasonUnknown())
      case Some(false) => Array("Unsatisfiable.")
      case Some(true) => {
        parseResult(goal)(solver.getModel(), vars)
      }
    }
  }


}
