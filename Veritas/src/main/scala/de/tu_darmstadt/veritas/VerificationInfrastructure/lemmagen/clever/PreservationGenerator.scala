package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem, Query, Refinement}
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

class PreservationGenerator(val problem: Problem, function: FunctionDef, predicate: FunctionDef) {
  import Query._

  implicit private val enquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head // TODO
  }

  def generateBase(): Lemma = {
    // --------------------
    // [predicate]([], ...)
    val outType = function.successfulOutType
    // [producer]([], ...) =  []
    // producer arguments can be fresh or bound with matching types
    val outVar :: inVars = Assignments.generateSimpleSingle(outType +: function.inTypes)
    // the success variable can be any of the arguments of ``predicate``, with matching types
    val predicateArgs = generatePredicateArguments(outVar)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we find all inVars with matching type
    val matchingInVars = inVars.filter(_.sortType == outType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(inVars), outVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    for(inVar <- matchingInVars) {
      val constraints = predicate.inTypes.map(inType =>
        if(inType == inVar.sortType)
          Constraint.fixed(inVar)
        else
          Constraint.fresh(inType)
      )
      val assignment = Assignments.generate(constraints, lemma).head
      val refinement = Refinement.Predicate(predicate, Assignments.wrapMetaVars(assignment))
      lemma = refinement.refine(problem, lemma).getOrElse(lemma)
    }
    lemma
  }

  def restrictableVariables(lemma: Lemma): Set[MetaVar] = lemma.boundVariables.filterNot(_.sortType == termType)
  def preVariables(lemma: Lemma): Set[MetaVar] = lemma.boundVariables -- postVariables(lemma)
  def postVariables(lemma: Lemma): Set[MetaVar] = FreeVariables.freeVariables(lemma.consequences)

  def generateEquations(lemma: Lemma): Set[Refinement] = {
    val restrictable = restrictableVariables(lemma)
    val partitioned = restrictable.groupBy(_.sortType)
    /*var refinements = new mutable.List[Refinement]()
    for((typ, metaVars) <- partitioned) {
      if(metaVars.size > 1) {
        val equals = metaVars.subsets.filter(_.size >= 2)
      }
    }*/
    Set()
  }

  def generateRestrictions(lemma: Lemma): Set[Refinement] = {
    val restrictable = restrictableVariables(lemma)
    print(restrictable)
    generateEquations(lemma)
    Set()
  }

  def generate(): Seq[Lemma] = {
    val tree = new RefinementTree(generateBase())
    println(tree.root.lemma)
    /*val refinement = Refinement.Equation(MetaVar("tt"), FunctionMeta(MetaVar("tt2")))
    val foo = tree.root.refine(problem, refinement)
    val refinement2 = Refinement.Equation(MetaVar("tt"), FunctionMeta(MetaVar("tt1")))
    foo.refine(problem, refinement2)*/
    generateRestrictions(tree.root.lemma)
    tree.prune(problem, tree.nodes.toSeq)
    tree.visualizeRT(new File("preservation.png"), "png")

    Seq()
  }
}
