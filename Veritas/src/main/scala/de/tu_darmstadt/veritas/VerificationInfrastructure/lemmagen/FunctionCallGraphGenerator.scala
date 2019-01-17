package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq, FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.scalaspl.util.{AugmentedCallGraph, VeritasAugmentedCallGraph, VeritasAugmentedCallGraphBuilder}

import scala.collection.mutable

class FunctionCallGraphGenerator(problem: Problem) {
  type VeritasACG = AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta]

  val augmentedCallGraphs = new mutable.HashMap[FunctionDef, VeritasACG]

  def lookupFunction(name: String): Option[FunctionDef] = {
    problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions ++ problem.dsk.staticFunctions, name)
  }

  def generateAugmentedCallGraph(function: FunctionDef): VeritasACG = {
    new VeritasAugmentedCallGraphBuilder(problem.spec).translate(function)(
      VeritasAugmentedCallGraph(function.signature.name)
    )
  }

  def getAugmentedCallGraph(function: FunctionDef): VeritasACG = {
    if(!augmentedCallGraphs.contains(function))
      augmentedCallGraphs(function) = generateAugmentedCallGraph(function)
    augmentedCallGraphs(function)
  }

  def getInvokedFunctions(function: FunctionDef): Set[FunctionDef] = {
    val acg = getAugmentedCallGraph(function)
    acg.nodes.collect {
      case acg.FunctionCall(_, _, name) => lookupFunction(name)
    }.flatten.toSet
  }

  def generate(entryPoint: FunctionDef): Map[FunctionDef, Set[FunctionDef]] = {
    val result = new mutable.HashMap[FunctionDef, Set[FunctionDef]]()
    var queue = new mutable.Queue[FunctionDef]()
    queue += entryPoint
    while(queue.nonEmpty) {
      val function = queue.dequeue()
      val invokedFunctions = getInvokedFunctions(function)
      result(function) = invokedFunctions
      queue ++= invokedFunctions.filterNot(result.contains)
    }
    result.toMap
  }
}

object FunctionCallGraphGenerator {
  def makeDotString(fct: Map[FunctionDef, Set[FunctionDef]]): String = {
    val builder = StringBuilder.newBuilder
    for((caller, callees) <- fct) {
      builder.append(s"${caller.signature.name} [shape=box];\n")
    }
    for((caller, callees) <- fct) {
      for(callee <- callees) {
        builder.append(s"${caller.signature.name} -> ${callee.signature.name};\n")
      }
    }
    s"digraph {\n ${builder.toString} }"
  }
}