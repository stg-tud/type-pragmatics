package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Solve, StructInductCase, StructuralInduction}
import de.tu_darmstadt.veritas.backend.ast.{MetaVar, VeritasConstruct, VeritasFormula}

class QLSoundnessProofGraph(file: File) {
  import QLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
  SQLSoundnessProofGraph.initializeGraphTypes(g)

  val specenq = new VeritasSpecEnquirer(fullQLspec)

  //progress root obligation
  val progressObligation: g.Obligation = g.newObligation(fullQLspec, QLProgress)
  g.storeObligation("QL progress", progressObligation)

  private val rootInduction = StructuralInduction(MetaVar("q"), fullQLspec, specenq)
  // first proof step: structural induction
  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInduction)

  val rootobl = g.findObligation("QL progress").get
  val rootsubobs = g.requiredObls(rootinductionPS)
  val casenames = rootInduction.enumerateCaseNames[g.Obligation](rootsubobs)
  val caseedges: Seq[StructInductCase[VeritasConstruct, VeritasFormula]] =
    (rootInduction.enumerateCases(rootsubobs) map
      {case (k, v) => v._2.asInstanceOf[StructInductCase[VeritasConstruct, VeritasFormula]]}).toSeq

  //apply simply Solve-tactic to qempty base case
  val qemptycaseobl = rootInduction.selectCase(casenames(0), rootsubobs)
  val qemptycasePS = g.applyTactic(qemptycaseobl, Solve[VeritasConstruct, VeritasFormula])

  // TODO apply CaseDistinction to qempty case
  val qsinglecaseobl = rootInduction.selectCase(casenames(1), rootsubobs)

  // TODO apply CaseDistinction to qcond case
  val qcondcaseobl = rootInduction.selectCase(casenames(2), rootsubobs)

  // TODO apply CaseDistinction to qseq case
  val qseqcaseobl = rootInduction.selectCase(casenames(3), rootsubobs)

  //apply simply Solve-tactic to qgroup base case
  val qgroupcaseobl = rootInduction.selectCase(casenames(4), rootsubobs)
  val qgroupcasePS = g.applyTactic(qgroupcaseobl, Solve[VeritasConstruct, VeritasFormula])
}

// Executing this object creates a new QL Soundness Proof Graph,
// attempting to verify as much as possible
object ConstructQLSoundnessGraph extends App {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val file = new File("QLSoundnessProofGraph-store")
  recursivedelete(file) //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for QLSoundnessProofGraph-store.")

  val pg = new QLSoundnessProofGraph(file)

  //pg.verifySingleStepsSimple()

  val rootobl = pg.g.findObligation("QL progress").get
  val subobs = pg.g.requiredObls(pg.rootinductionPS)

  println("Root obligation: " + rootobl)
  println("Induction cases:")
  println(subobs)

}
