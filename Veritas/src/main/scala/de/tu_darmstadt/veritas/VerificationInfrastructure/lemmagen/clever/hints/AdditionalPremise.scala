package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.backend.ast
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.scalaspl.translator.FunctionExpressionTranslator

import scala.collection.mutable
import scala.meta.{Term, Tree}
import scala.meta.inputs.Input
import scala.meta.transversers.Traverser

class CollectMetaVarsTraverser extends Traverser {
  val metaVariables = new mutable.HashSet[String]()

  override def apply(tree: Tree): Unit = tree match {
    case Term.ApplyInfix(lhs, _, _, rhs) =>
      apply(lhs)
      apply(rhs)
    case Term.Apply(_, args) =>
      apply(args)
    case Term.Name(n) => metaVariables += n
    case node => super.apply(node)
  }
}

object CollectMetaVarsTraverser {
  def collect(tree: Tree): Set[String] = {
    val traverser = new CollectMetaVarsTraverser()
    traverser.apply(tree)
    traverser.metaVariables.toSet
  }
}

class AdditionalPremise(problem: Problem, hint: String) extends Hint {


  def constructPremise(lemma: Lemma): TypingRuleJudgment = {
    val input = Input.VirtualFile("annotation/hint.scala", hint)
    val tree = input.parse[scala.meta.Term].get
    val metaVars = CollectMetaVarsTraverser.collect(tree)
    val translator = new FunctionExpressionTranslator(metaVars.toSeq)
    val translated = translator.translateExp(tree)
    val judgment = FunctionExpJudgment(translated)
    // rename placeholder vars
    val varTypes = problem.enquirer.getAllVarTypes(judgment)
    val placeholderVars = metaVars.filter(_.startsWith("_")).map(MetaVar(_)).toList
    if(placeholderVars.nonEmpty) {
      val constraints = placeholderVars.map(mv => Constraint.fresh(varTypes(mv)))
      val assignment = Assignments.generate(constraints, lemma).head
      val renaming = placeholderVars.zip(assignment).toMap
      LemmaEquivalence.VariableRenamer(judgment, renaming.withDefault(d => d))
    } else {
      judgment
    }
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
