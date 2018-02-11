package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionExpEq, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, VeritasConstruct, VeritasFormula}

class QLSoundnessProofGraph(file: File) {
  import QLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
  SQLSoundnessProofGraph.initializeGraphTypes(g)

  val specenq = new VeritasSpecEnquirer(fullQLspec)

  def getCases(inductionVar: MetaVar, goal: VeritasFormula): Seq[VeritasFormula] = {
    val goalBody = specenq.getQuantifiedBody(goal)
    val ivCases = specenq.getCases(inductionVar, goalBody) map { ic =>
      specenq.assignCaseVariables(ic, goalBody)
    }
    ivCases map (specenq.makeEquation(inductionVar, _))
  }

  // TODO maybe there can be other matching conditions like !app(~metavar) or app(~metavar)
  def getNextMatchingVariables(matchingCondition: VeritasFormula): Seq[MetaVar] = {
    matchingCondition.asInstanceOf[FunctionExpJudgment] match {
      case FunctionExpJudgment(FunctionExpEq(rhs, FunctionExpApp(_, args))) =>
        args.collect { case mv: FunctionMeta => mv.metavar }
      case _ => Seq()
    }
  }

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

  val matchingConds = getCases(MetaVar("q"), rootobl.goal)
  val qsinglecaseobl = rootInduction.selectCase(casenames(1), rootsubobs)

  val qsingleCaseDistinction = StructuralCaseDistinction(getNextMatchingVariables(matchingConds(1)).head, fullQLspec, specenq)
  val qsinglecasePS = g.applyTactic(qsinglecaseobl, qsingleCaseDistinction)

  val qsinglesubobs = g.requiredObls(qsinglecasePS)
  val qsinglequestion = g.applyTactic(qsinglesubobs.toSeq(0)._1, Solve[VeritasConstruct, VeritasFormula])
  val qsingledefquestion = g.applyTactic(qsinglesubobs.toSeq(1)._1, Solve[VeritasConstruct, VeritasFormula])


  // TODO apply CaseDistinction to qseq case
  val qseqcaseobl = rootInduction.selectCase(casenames(2), rootsubobs)
  val qseqCaseDistinction = EqualityCaseDistinction(getNextMatchingVariables(matchingConds(2)).head, FunctionExpApp("qempty", Nil), fullQLspec, specenq)
  val qseqcasePS = g.applyTactic(qseqcaseobl, qseqCaseDistinction)

  // TODO apply CaseDistinction to qcond case
  val qcondcaseobl = rootInduction.selectCase(casenames(3), rootsubobs)

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
