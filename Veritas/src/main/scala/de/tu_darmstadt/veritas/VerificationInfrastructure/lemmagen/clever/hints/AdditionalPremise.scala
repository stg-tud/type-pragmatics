package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.scalaspl.translator.FunctionExpressionTranslator

import scala.meta.Term
import scala.meta.inputs.Input

class AdditionalPremise(problem: Problem, hint: String) extends Hint {
  def constructPremise(lemma: Lemma): TypingRuleJudgment = {
    val input = Input.VirtualFile("annotation/hint.scala", hint)
    val tree = input.parse[scala.meta.Term].get

    var currentIndex = 1
    var vars = Seq[MetaVar]()
    def nextFreshName(): String = {
      val name = s"_$currentIndex"
      vars +:= MetaVar(name)
      currentIndex += 1
      name
    }
    val transformed = tree.transform {
      case Term.Placeholder() => Term.Name(nextFreshName())
    }.asInstanceOf[Term]
    println(vars)
    val translator = new FunctionExpressionTranslator(vars.map(_.name))
    val judgment = FunctionExpJudgment(translator.translateExp(transformed))
    val varTypes = problem.enquirer.getAllVarTypes(judgment)
    val constraints = vars.map(mv => Constraint.fresh(varTypes(mv)))
    val assignment = Assignments.generate(constraints, lemma).head
    val renaming = vars.zip(assignment).toMap
    val renamed = LemmaEquivalence.VariableRenamer(judgment, renaming)
    renamed
  }

  override def apply(lemma: Lemma,
                     post: Set[MetaVar],
                     constrained: Set[MetaVar]): (Lemma, Set[MetaVar], Set[MetaVar]) = {
    val newPremise = constructPremise(lemma)
    (lemma.addPremise(newPremise),
      post,
      constrained ++ FreeVariables.freeVariables(newPremise, Set.empty[MetaVar]))
  }
}
