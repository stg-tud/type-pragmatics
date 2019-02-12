package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.backend.ast
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
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

class AdditionalPremises(problem: Problem, premiseHints: Seq[String]) extends Hint {
  override def apply(baseLemma: Lemma,
                     post: Set[MetaVar],
                     constrained: Set[MetaVar]): (Lemma, Set[MetaVar], Set[MetaVar]) = {
    val allMetaVars = new mutable.HashSet[MetaVar]()
    var lemma = baseLemma
    for(hint <- premiseHints) {
      val input = Input.VirtualFile("annotation/hint.scala", hint)
      val tree = input.parse[scala.meta.Term].get
      val metaVars = CollectMetaVarsTraverser.collect(tree)
      allMetaVars ++= metaVars.map(MetaVar(_))
      val translator = new FunctionExpressionTranslator(metaVars.toSeq)
      val translated = translator.translateExp(tree)
      val judgment = FunctionExpJudgment(translated)
      lemma = lemma.addPremise(judgment)
    }
    // rename placeholder vars
    val varTypes = problem.enquirer.getAllVarTypes(lemma)
    val placeholderVars = allMetaVars.filter(_.name.startsWith("_")).toList
    val constraints = placeholderVars.map(mv => Constraint.fresh(varTypes(mv)))
    val assignment = Assignments.generate(constraints, varTypes.keySet).head
    val renaming = placeholderVars.zip(assignment).toMap.withDefault(d => d)
    val newConstrainedVars = allMetaVars.map(renaming) intersect baseLemma.boundVariables
    val newLemma = Lemma.fromTypingRule(LemmaEquivalence.renameVariables(lemma, renaming))
    (newLemma,
      post,
      constrained ++ newConstrainedVars)
  }
}

object AdditionalPremises {
  def fromDSK(problem: Problem, function: FunctionDef): AdditionalPremises = {
    val premises = problem.dsk.additionalPremises.getOrElse(function, Seq())
    new AdditionalPremises(problem, premises)
  }
}