package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Equation, Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.util.FreeVariables

object Variables {
  private def replace(set: Set[MetaVar], element: MetaVar, replacement: MetaVar): Set[MetaVar] = {
    set.map(mv =>
      if(element == mv)
        replacement
      else
        mv
    )
  }

  def calculatePostVariables(post: Set[MetaVar], refinement: Refinement): Set[MetaVar] = {
    refinement match {
      case Predicate(fn, args) => post
      case SuccessfulApplication(fn, args, result) => post + result
      case Equation(left, FunctionMeta(rightVar)) => replace(post, rightVar, left)
      case Equation(left, right) =>
        sys.error("not sure what to do") // TODO
        post
    }
  }

  def calculateConstrainedVariables(constrainedVariables: Set[MetaVar], refinement: Refinement): Set[MetaVar] = {
    refinement match {
      case Predicate(fn, args) =>
        constrainedVariables ++ args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar]))
      case SuccessfulApplication(fn, args, result) =>
        constrainedVariables ++ args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar])) + result
      case Equation(left, FunctionMeta(rightVar)) => replace(constrainedVariables, rightVar, left)
      case Equation(left, right) =>
        sys.error("not sure what to do") // TODO
    }
  }
}
