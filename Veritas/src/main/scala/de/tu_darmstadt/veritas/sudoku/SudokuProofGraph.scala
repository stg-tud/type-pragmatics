package de.tu_darmstadt.veritas.sudoku

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.Strategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Finished
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraphTraversals, ProofGraphXodus, PropertyTypes}
import de.tu_darmstadt.veritas.sudoku.tactics._

class EmptySpec extends Comparable[EmptySpec] with Serializable {
  override def compareTo(o: EmptySpec): Int = this.hashCode() compare o.hashCode()
}

/**
  * controls construction of a sudoku proof graph
  * by applying domain-specific strategies
  */
class SudokuProofGraph(file: File, initialfield: SudokuField, rootstrategy: Strategy[EmptySpec, SudokuField]) {

  val g: ProofGraphXodus[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField] =
    new ProofGraphXodus[EmptySpec, SudokuField](file) with ProofGraphTraversals[EmptySpec, SudokuField]
  SudokuProofGraph.initializeGraphTypes(g)

  val initialobligation: g.Obligation = g.newObligation(new EmptySpec, initialfield)
  g.storeObligation("initial", initialobligation)

  def constructPG(): Unit = rootstrategy.applyToPG(g)(initialobligation)

  def verifyStepsSolveLeaves(): Unit = {
    val intermediateStepVerifier = new SudokuIntermediateStepVerifier
    val leafVerifier = new SudokuLeafVerifier
    val proofsteps = g.proofstepsDFS()
    for (ps <- proofsteps) {
      ps.tactic match {
        case SolveSudoku => g.verifyProofStep(ps, leafVerifier)
        case _ => g.verifyProofStep(ps, intermediateStepVerifier)
      }
    }
  }

  def printSteps(): String = {
    val obligations = g.obligationDFS()
    (for (o <- obligations) yield {
      val state = "Sudoku solving state: \n" +
        o.goal.printWithCandidates() + "\n"
      val proofstep = g.appliedStep(o)
      val step: String = (proofstep map (s => s.tactic.toString)).getOrElse("") + "\n"
      val edge = proofstep map (g.requiredObls(_))
      val edgestring: String = (if (edge.isDefined && edge.get.size == 1) edge.get.head._2.desc else "") + "\n"
      val verificationstat = proofstep flatMap (g.verifiedBy(_))
      val vsstring : String = ("Verifier result: " + (if (verificationstat.isDefined)
        verificationstat.get.status match {
          case f: Finished[EmptySpec, SudokuField] => f.status.proverResult.fullLogs
          case s => s.toString
        } else "none")) + "\n"
      state + step + edgestring + vsstring
    }).mkString("\n")
  }

  def printLastStep(): String = {
    val ps_last = g.proofstepsDFS().last
    val verificationstat = g.verifiedBy(ps_last)
    "Verifier result: " + (verificationstat.get.status match {
      case f: Finished[EmptySpec, SudokuField] => f.status.proverResult.fullLogs
      case s => s.toString
    })
  }

}


object SudokuProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[EmptySpec, SudokuField]) = {
    PropertyTypes.registerPropertyType[SudokuField](g.store)
    PropertyTypes.registerPropertyType[EmptySpec](g.store)
    PropertyTypes.registerPropertyType[SolveSudoku.type](g.store)
    PropertyTypes.registerPropertyType[RuleOutCandidatesSimple.type](g.store)
    PropertyTypes.registerPropertyType[SimpleRuleOut](g.store)
    PropertyTypes.registerPropertyType[SolveSingleCandidate.type](g.store)
    PropertyTypes.registerPropertyType[FillSingleCandidate](g.store)
    PropertyTypes.registerPropertyType[Finished[EmptySpec, SudokuField]](g.store)
    PropertyTypes.registerPropertyType[Z3Evidence](g.store)
    PropertyTypes.registerPropertyType[SudokuLeafVerifier](g.store)
    PropertyTypes.registerPropertyType[SudokuIntermediateStepVerifier](g.store)
    PropertyTypes.registerPropertyType[NakedGroup](g.store)
    PropertyTypes.registerPropertyType[FoundNakedGroup](g.store)
    PropertyTypes.registerPropertyType[OnlyCellWithCandidate.type](g.store)

  }
}
