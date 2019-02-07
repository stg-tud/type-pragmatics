package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Refinement.{Equation, Predicate, SuccessfulApplication}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.util.FreeVariables

object PrePostVariables {
  private def replace(set: Set[MetaVar], element: MetaVar, replacement: MetaVar): Set[MetaVar] = {
    set.map(mv =>
      if(element == mv)
        replacement
      else
        mv
    )
  }

  def calculatePrePostVariables(pre: Set[MetaVar],
                                post: Set[MetaVar],
                                refinement: Refinement): (Set[MetaVar], Set[MetaVar]) = {
    refinement match {
      case Predicate(fn, args) =>
        val freeVariables = args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar]))
        (pre ++ freeVariables, post)
      case SuccessfulApplication(fn, args, result) =>
        val freeVariables = args.flatMap(FreeVariables.freeVariables(_, Set.empty[MetaVar]))
        (pre ++ freeVariables, post + result)
      case Equation(left, FunctionMeta(rightVar)) =>
        (replace(pre, rightVar, left), replace(post, rightVar, left))
      case Equation(left, right) =>
        sys.error("not sure what to do") // TODO
        (pre, post)
    }
  }
}
