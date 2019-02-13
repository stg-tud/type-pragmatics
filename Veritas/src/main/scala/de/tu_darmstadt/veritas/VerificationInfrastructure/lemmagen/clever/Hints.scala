package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.scalaspl.translator.FunctionExpressionTranslator

import scala.collection.mutable
import scala.meta.inputs.Input
import scala.meta.transversers.Traverser
import scala.meta.{Term, Tree}

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

class Hints(problem: Problem, premiseHints: Seq[String], irrelevantVariables: Seq[String]) {
  def apply(annotatedLemma: AnnotatedLemma): AnnotatedLemma = annotatedLemma match {
    case AnnotatedLemma(baseLemma, constrainedVariables, postVariables) =>
      val allMetaVars = new mutable.HashSet[MetaVar]()
      var lemma = baseLemma
      for (hint <- premiseHints) {
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
      val newLemma = Lemma.fromTypingRule(LemmaEquivalence.renameVariables(lemma, renaming))
      val newIrrelevantVariables = irrelevantVariables.map(name => renaming(MetaVar(name)))
      AnnotatedLemma(newLemma, constrainedVariables ++ newIrrelevantVariables, postVariables)
  }
}

object Hints {
  def fromDSK(problem: Problem, function: FunctionDef): Hints = {
    val premises = problem.dsk.additionalPremises.getOrElse(function, Seq())
    val irrelevantVariables = problem.dsk.irrelevantVariables.getOrElse(function, Seq())
    new Hints(problem, premises, irrelevantVariables)
  }
}