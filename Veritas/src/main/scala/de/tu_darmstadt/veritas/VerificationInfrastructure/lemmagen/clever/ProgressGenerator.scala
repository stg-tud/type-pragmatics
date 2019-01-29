package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Choice, Constraint}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

import scala.collection.mutable

class ProgressGenerator(val problem: Problem, function: FunctionDef) extends StrategyHelpers {
  import Query._

  implicit private val enquirer = problem.enquirer

  def generateBase(): Seq[Lemma] = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successVar :: arguments = Assignments.generateSimpleSingle(function.successfulOutType +: function.inTypes)
    val invocationExp = FunctionExpApp(function.name, Assignments.wrapMetaVars(arguments))
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    Seq(new Lemma(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  def constructAllChoices[T](choices: Seq[Seq[T]]): Seq[Seq[T]] = choices match {
    case currentChoices :: remainingChoices =>
      val constructedRemainingChoices: Seq[Seq[T]] = constructAllChoices(remainingChoices)
      for(currentChoice <- currentChoices; remainingChoice <- constructedRemainingChoices)
          yield currentChoice +: remainingChoice
    case Nil => Seq(Seq())
  }

  def constrainConsequenceVariables(lemma: Lemma): Set[Lemma] = {
    val consequenceVariables: Seq[MetaVar] = lemma.consequences.flatMap(
      enquirer.getUniversallyQuantifiedVars(_)
    ).collect {
      case mv@MetaVar(_) => mv
    }
    // find all static predicates and transformers that talk about these variables
    val varConstraints = consequenceVariables.map(mv =>
      (enquirer.retrievePredicates(Set(mv.sortType)) ++ enquirer.retrieveTransformers(Set(mv.sortType)))
        .filter(_.isStatic).toSeq)
    val choices = constructAllChoices(varConstraints)
    var lemmas = new mutable.HashSet[Lemma]()
    for(choice <- choices) {
      var currentLemmas = Set(lemma)
      for((mv, fn) <- consequenceVariables zip choice) {
        val assignmentConstraint = fn.inTypes.map {
          case typ if typ == mv.sortType => Constraint.fixed(mv) // TODO: might be multiple
          case typ => Constraint.fresh(typ)
        }
        currentLemmas = currentLemmas.flatMap(lemma => refine(lemma, fn.successfulOutType match {
            case SortRef("Bool") => selectPredicate(lemma, fn, assignmentConstraint)
            case typ => selectSuccessfulApplication(lemma, fn, assignmentConstraint, Constraint.fresh(typ))
          }))
      }
      lemmas ++= currentLemmas
    }
    lemmas.toSet
  }

  def addEquations(lemma: Lemma): Set[Lemma] = {
    buildTree(lemma)
    Set(lemma)
  }

  class ProgressRefinementTree(rootLemma: Lemma, val rootTag: LemmaTag) extends RefinementTree[LemmaTag](rootLemma, rootTag) {
    def refine(node: Node, refinement: Refinement): Node = {
      node.findRefinement(refinement) match {
        case Some(child) => child
        case None =>
          refinement.refine(problem, node.lemma) match {
            case Some(refinedLemma) =>
              val child = new Node(refinedLemma, Some(refinement), LemmaTag(None)) // TODO
              node.addChild(child)
              child
            case None => node
          }
        }
      }

    override def makeDotStringForNode(node: Node, sb: StringBuilder, nodeID: String): Unit = {
      val color = node.tag.status match {
        case Some(Oracle.Inconclusive()) => "gray"
        case Some(Oracle.ProvablyFalse(_)) => "red"
        case _ => "black"
      }
      val label = "\"" + node.lemma.refinements.last.toString + "\""
      sb.append(nodeID + s" [shape=box, label=$label, color=$color];\n")
    }

    def prune(nodes: Seq[Node]) = {
      val unknownLemmas = nodes.collect {
        case node if node.tag.status.isEmpty => node.lemma
      }.toSet
      val remaining = Oracle.pruneProvablyFalseLemmas(problem, unknownLemmas)
      for(node <- nodes) {
        if(remaining contains node.lemma) {
          node.tag = node.tag.copy(Some(Oracle.Inconclusive()))
          println(s"set to INCONCLUSIVE: ${node.lemma}")
        } else {
          node.tag = node.tag.copy(Some(Oracle.ProvablyFalse(None)))
          println(s"set to FALSE: ${node.lemma}")
        }
      }
    }
  }

  case class LemmaTag(status: Option[Oracle.Answer])
  trait Equation {}
  case class VarVarEquation(left: MetaVar, right: MetaVar) extends Equation
  case class VarConstructorEquation(left: MetaVar, right: DataTypeConstructor, args: Seq[FunctionMeta]) extends Equation

  def getConstrainedVariables(lemma: Lemma): Set[MetaVar] =
    enquirer.getUniversallyQuantifiedVars(lemma.consequences.head).asInstanceOf[Set[MetaVar]]

  def getEquations(lemma: Lemma, metaVar: MetaVar): Set[Equation] = {
    val equations = new mutable.HashSet[Equation]()
    equations ++= lemma.bindingsOfType(metaVar.sortType).filterNot(_ == metaVar).map(right => VarVarEquation(metaVar, right
))
    // all zero-argument constructors of value
    val constructors = enquirer.getConstructors(metaVar.sortType).filter(_.in.isEmpty)
    constructors.foreach { constructor =>
      equations += VarConstructorEquation(metaVar, constructor, Seq())
    }
    // find datatype constructors of metaVar.sortType
    equations.toSet
  }

  def buildTree(lemma: Lemma): Unit = {
    val tree = new ProgressRefinementTree(lemma, LemmaTag(None))
    lemma.boundVariables.foreach { mv =>
      tree.leaves.foreach { leaf =>
        val equations = getEquations(leaf.lemma, mv)
        val possibleVariableEquations = equations.collect {
          case VarVarEquation(_, right) => right
        }
        val possibleConstrEquations = equations.collect {
          case VarConstructorEquation(_, right, _) => right
        }
        val nodesToRefine = new mutable.ListBuffer[tree.Node]()
        possibleVariableEquations.subsets.foreach { rightSides =>
          if (rightSides.isEmpty)
            Set(lemma)
          else {
            rightSides.foreach { right =>
              val refinement = Refinement.Equation(mv, FunctionMeta(right))
              nodesToRefine += tree.refine(leaf, refinement)
            }
          }
        }
        nodesToRefine.foreach { child =>
          possibleConstrEquations.foreach { constr =>
            val refinement = Refinement.Equation(mv, FunctionExpApp(constr.name, Seq()))
            tree.refine(child, refinement)
          }
        }
      }
    }
    tree.prune(tree.leaves.toSeq)
    tree.visualizeRT(new File(s"rt-${lemma.name}.png"))
  }
}
