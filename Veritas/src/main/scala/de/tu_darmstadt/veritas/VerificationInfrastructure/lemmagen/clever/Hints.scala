package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.scalaspl.translator.FunctionExpressionTranslator

import scala.collection.mutable
import scala.meta.inputs.Input
import scala.meta.transversers.Traverser
import scala.meta.{Term, Tree}

/** ScalaMeta AST Traverser that finds all meta variables */
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

/** Find all metavariables that are mentioned in a a given ScalaMeta tree */
object CollectMetaVarsTraverser {
  def collect(tree: Tree): Set[String] = {
    val traverser = new CollectMetaVarsTraverser()
    traverser.apply(tree)
    traverser.metaVariables.toSet
  }
}

/** A hint encapsulates a sequence of additional premises and a sequence of irrelevant variables.
  * A hint can be applied to an annotated lemma.
  */
class Hints(problem: Problem, premiseHints: Seq[String], irrelevantVariables: Seq[String]) {
  def apply(annotatedLemma: AnnotatedLemma): AnnotatedLemma = annotatedLemma match {
    case AnnotatedLemma(baseLemma, constrainedVariables, postVariables) =>
      // we collect all metavariables that are mentioned in additional premises
      val allMetaVars = new mutable.HashSet[MetaVar]()
      // first, add the additional premises mentioned in `premiseHints`
      var lemma = baseLemma
      for (hint <- premiseHints) {
        // we parse the string to a ScalaMeta Tree
        val input = Input.VirtualFile("annotation/hint.scala", hint)
        val tree = input.parse[scala.meta.Term].get
        val metaVars = CollectMetaVarsTraverser.collect(tree)
        allMetaVars ++= metaVars.map(MetaVar(_))
        // we translate ScalaMeta Tree to a Veritas AST
        val translator = new FunctionExpressionTranslator(metaVars.toSeq)
        val translated = translator.translateExp(tree)
        // ... and add a corresponding premise
        val judgment = FunctionExpJudgment(translated)
        lemma = lemma.addPremise(judgment)
      }
      // determine the types of all metavariables mentioned in the augmented lemma
      val varTypes = problem.enquirer.getAllVarTypes(lemma)
      // generate fresh variable names for all placeholder variables (variables starting with _)
      val placeholderVars = allMetaVars.filter(_.name.startsWith("_")).toList
      val constraints = placeholderVars.map(mv => Constraint.fresh(varTypes(mv)))
      val assignment = Assignments.generate(constraints, varTypes.keySet).head
      val renaming = placeholderVars.zip(assignment).toMap.withDefault(d => d)
      // rename placeholder variable names in the lemma accordingly
      val newLemma = Lemma.fromTypingRule(LemmaEquivalence.renameVariables(lemma, renaming))
      // rename the placeholder variable names in the set of irrelevant variables
      val newIrrelevantVariables = irrelevantVariables.map(name => renaming(MetaVar(name)))
      // return the augmented lemma, the augmented set of constrained variables and the set of post variables
      AnnotatedLemma(newLemma, constrainedVariables ++ newIrrelevantVariables, postVariables)
  }
}

object Hints {
  /** Construct a hint that leaves the annotated lemma unchanged */
  def empty(problem: Problem): Hints = new Hints(problem, Seq(), Seq())

  /** Return a Hints instance that models the lemma generator hints specified for `function` and `tag`.
    * If lemma generation for `function` and `tag` is suppressed by a hint, return None.
    */
  def fromDSK(problem: Problem, function: FunctionDef, tag: Seq[String]): Option[Hints] = {
    parseLemmaGeneratorHints(
      problem,
      tag,
      problem.dsk.lemmaGeneratorHints.getOrElse(function, Seq()))
  }

  /** Return a Hints instance that implements subset of `hints` that matches `tag`.
    * If any matching hint suppresses lemma generation for `tag`, return None. */
  def parseLemmaGeneratorHints(problem: Problem,
                               tag: Seq[String],
                               hints: Seq[(Seq[String], Seq[String], Seq[String], Boolean)]): Option[Hints] = {
    // find all matching hints: these are all hints whose tag is a premise of `tag`
    val matchingHints = hints.collect {
      case (pattern, localAdditionalPremises, localIrrelevantVariables, suppress) if tag.startsWith(pattern) =>
        (localAdditionalPremises, localIrrelevantVariables, suppress)
    }
    // find out whether any hint has suppress = true
    if(matchingHints.exists(_._3)) {
      None
    } else {
      // collect all additionalPremises and irrelevantVariables of matching hints
      val additionalPremises = matchingHints.flatMap(_._1)
      val irrelevantVariables = matchingHints.flatMap(_._2)
      Some(new Hints(problem, additionalPremises, irrelevantVariables))
    }
  }
}