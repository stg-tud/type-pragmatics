package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._

class QLSoundnessProofGraph(file: File) {
  import QLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
  SQLSoundnessProofGraph.initializeGraphTypes(g)

  val specenq = new VeritasSpecEnquirer(fullQLspec)

  def getCases(metaVar: MetaVar, goal: VeritasFormula): Seq[VeritasConstruct] = {
    val goalBody = specenq.getQuantifiedBody(goal)
    val ivCases = specenq.getCases(metaVar, goalBody) map { ic =>
      specenq.assignCaseVariables(ic, goalBody)
    }
    ivCases
  }

  def getIntroducedMetaVars(expression: VeritasConstruct): Seq[MetaVar] = {
    def collectMetaVars(args: Seq[FunctionExpMeta]): Seq[MetaVar] =
      args.collect { case mv: FunctionMeta => mv.metavar }

    expression.asInstanceOf[FunctionExp] match {
      //case FunctionExpJudgment(FunctionExpEq(rhs, FunctionExpApp(_, args))) => collectMetaVars(args)
      //case FunctionExpJudgment(FunctionExpNeq(rhs, FunctionExpApp(_, args))) => collectMetaVars(args)
      case FunctionExpApp(_, args) => collectMetaVars(args)
      case FunctionExpNot(FunctionExpApp(_, args)) => collectMetaVars(args)
      case _ => Seq()
    }
  }

  //progress root obligation
  val progressObligation: g.Obligation = g.newObligation(fullQLspec, QLProgress)
  g.storeObligation("QL progress", progressObligation)

  private val rootInduction = StructuralInduction(MetaVar("q"), fullQLspec, specenq)
  // first proof step: structural induction
  //val rootPS = g.applyTactic(progressObligation, Solve[VeritasConstruct, VeritasFormula])
  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInduction)

  val rootobl = g.findObligation("QL progress").get
  val rootsubobs = g.requiredObls(rootinductionPS)
  val casenames = rootInduction.enumerateCaseNames[g.Obligation](rootsubobs)
  val caseedges: Seq[StructInductCase[VeritasConstruct, VeritasFormula]] =
    (rootInduction.enumerateCases(rootsubobs) map
      {case (k, v) => v._2.asInstanceOf[StructInductCase[VeritasConstruct, VeritasFormula]]}).toSeq
  val matchingConds = getCases(MetaVar("q"), rootobl.goal)

  //apply simply Solve-tactic to qempty base case
  val qemptyObl = rootInduction.selectCase(casenames(0), rootsubobs)
  val qemptyPS = g.applyTactic(qemptyObl, Solve[VeritasConstruct, VeritasFormula])

  val qsingleObl = rootInduction.selectCase(casenames(1), rootsubobs)
  val qsingleCaseDistinction = StructuralCaseDistinction(getIntroducedMetaVars(matchingConds(1)).head, fullQLspec, specenq)
  val qsinglePS = g.applyTactic(qsingleObl, qsingleCaseDistinction)

  val qsinglesubobs = g.requiredObls(qsinglePS)
  val qsingleMatchingConds = getCases(getIntroducedMetaVars(matchingConds(1)).head, qsinglesubobs.toSeq(1)._1.goal)

  val questionPS = g.applyTactic(qsinglesubobs.toSeq(0)._1, Solve[VeritasConstruct, VeritasFormula])
  val valueCaseDistinction = BooleanCaseDistinction(FunctionExpApp("expIsValue", Seq(FunctionMeta(getIntroducedMetaVars(qsingleMatchingConds(1))(2)))), fullQLspec, specenq)

  val valuePS = g.applyTactic(qsinglesubobs.toSeq(1)._1, valueCaseDistinction)
  val valueCases = g.requiredObls(valuePS)

  val progressReduceExpLemmaApplication = LemmaApplication(Seq(ReduceExpProgress), fullQLspec, specenq)

  val expIsValueTruePS = g.applyTactic(valueCases.toSeq.head._1, Solve[VeritasConstruct, VeritasFormula])
  val expIsValueFalsePS = g.applyTactic(valueCases.toSeq.last._1, progressReduceExpLemmaApplication)
  val progressReduceExpSubs = g.requiredObls(expIsValueFalsePS)
  val reduceExpInduction = StructuralInduction(MetaVar("exp"), fullQLspec, specenq)

  val progressReduceExpPS = g.applyTactic(progressReduceExpSubs.toSeq.head._1, reduceExpInduction)

  val progressReduceExpCases = g.requiredObls(progressReduceExpPS).toSeq
  val progressReduceExpMatchingConds = getCases(MetaVar("exp"), ReduceExpProgress)

  val constantProgressReduceExpPS = g.applyTactic(progressReduceExpCases.head._1, Solve[VeritasConstruct, VeritasFormula])

  val progressLookupAnsMapLemma = LemmaApplication(Seq(LookupAnsMapProgress), fullQLspec, specenq)
  println(progressReduceExpCases(1)._1.goal)
  val qvarPS = g.applyTactic(progressReduceExpCases(1)._1, progressLookupAnsMapLemma)
  val progressLookupAnsMap = g.requiredObls(qvarPS).toSeq.head._1

  /*object progressLookupAnsMapInduction extends MockInduction(MetaVar("am")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val emptyObl = produce.newObligation(fullQLspec, LookupAnsMapProgressEmpty)
      val bindObl = produce.newObligation(fullQLspec, LookupAnsMapProgressAbind)

      Seq((emptyObl, StructInductCase[VeritasConstruct, VeritasFormula](LookupAnsMapProgressEmpty.goals.head.name,
        Seq(), Seq(), Seq())),
        (bindObl, StructInductCase[VeritasConstruct, VeritasFormula](LookupAnsMapProgressAbind.goals.head.name,
          Seq(FixedVar(LookupAnsMapProgressAbindConsts)),
          Seq(InductionHypothesis[VeritasFormula](LookupAnsMapProgressAbindIH)), Seq())))

    }
  }*/
  val progressLookupAnsMapInduction = StructuralInduction(MetaVar("am"), fullQLspec, specenq)
  val progressLookupAnsMapInductionPS = g.applyTactic(progressLookupAnsMap, progressLookupAnsMapInduction)
  val progressLookupAnsMapInductionCases = g.requiredObls(progressLookupAnsMapInductionPS).toSeq
  val progressLookupAnsMapInductionCasesPS =  progressLookupAnsMapInductionCases.map { case (obl, info) =>
    g.applyTactic(obl, Solve[VeritasConstruct, VeritasFormula])
  }


  val binopProgressReduceExpDistinction = BooleanCaseDistinction( FunctionExpAnd(
      FunctionExpApp("expIsValue", Seq(FunctionMeta(getIntroducedMetaVars(progressReduceExpMatchingConds(2)).head))),
        FunctionExpApp("expIsValue", Seq(FunctionMeta(getIntroducedMetaVars(progressReduceExpMatchingConds(2)).last)))
    ), fullQLspec, specenq)
  val binopProgressReduceExpPS = g.applyTactic(progressReduceExpCases(2)._1, binopProgressReduceExpDistinction)
  val binopProgressReduceExpCases = g.requiredObls(binopProgressReduceExpPS).toSeq
  val binopProgressReduceExpSomeExpPS = g.applyTactic(binopProgressReduceExpCases.head._1, Solve[VeritasConstruct, VeritasFormula])
  val binopProgressReduceExpNoExpPS = g.applyTactic(binopProgressReduceExpCases.last._1, Solve[VeritasConstruct, VeritasFormula])

  val unopProgressReduceExpDistinction = BooleanCaseDistinction(
    FunctionExpApp("expIsValue",
      Seq(FunctionMeta(getIntroducedMetaVars(progressReduceExpMatchingConds.last)(1)))), fullQLspec, specenq)
  val unopProgressReduceExpDisitinctionPS = g.applyTactic(progressReduceExpCases.last._1, unopProgressReduceExpDistinction)
  val unopProgressReduceExpCases = g.requiredObls(unopProgressReduceExpDisitinctionPS).toSeq
  val unopProgressReduceExpCasesPS = unopProgressReduceExpCases.map { case (obl, info) =>
    g.applyTactic(obl, Solve[VeritasConstruct, VeritasFormula])
  }

  val defquestionPS = g.applyTactic(qsinglesubobs.toSeq(2)._1, Solve[VeritasConstruct, VeritasFormula])

  val progressLookupQMapLemmaApplication = LemmaApplication(Seq(LookupQMapProgress), fullQLspec, specenq)
  val askPS = g.applyTactic(qsinglesubobs.toSeq(3)._1, progressLookupQMapLemmaApplication)
  val progressLookupQMap = g.requiredObls(askPS).toSeq.head._1

  /*object progressLookupQMapInduction extends MockInduction(MetaVar("qm")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {

      val emptyObl = produce.newObligation(fullQLspec, LookupQMapProgressEmpty)
      val bindObl = produce.newObligation(fullQLspec, LookupQMapProgressQmbind)

      Seq((emptyObl, StructInductCase[VeritasConstruct, VeritasFormula](LookupQMapProgressEmpty.goals.head.name,
        Seq(), Seq(), Seq())),
        (bindObl, StructInductCase[VeritasConstruct, VeritasFormula](LookupQMapProgressQmbind.goals.head.name,
          Seq(FixedVar(LookupQMapProgressQmbindConsts)),
          Seq(InductionHypothesis[VeritasFormula](LookupQMapProgressQmbindIH)), Seq())))

    }
  }*/
  val progressLookupQMapInduction = StructuralInduction(MetaVar("qm"), fullQLspec, specenq)

  val progressLookupQMapInductionPS = g.applyTactic(progressLookupQMap, progressLookupQMapInduction)
  val progressLookupQMapInductionCases = g.requiredObls(progressLookupQMapInductionPS).toSeq
  val progressLookupQMapInductionCasesPS = progressLookupQMapInductionCases.map { case (obl, _) =>
    g.applyTactic(obl, Solve[VeritasConstruct, VeritasFormula])
  }

  // apply CaseDistinction to qseq case
  val qseqObl = rootInduction.selectCase(casenames(2), rootsubobs)
  val qseqCaseDistinction = EqualityCaseDistinction(getIntroducedMetaVars(matchingConds(2)).head, FunctionExpApp("qempty", Nil), fullQLspec, specenq)
  val qseqcasePS = g.applyTactic(qseqObl, qseqCaseDistinction)

  val qseqsubobs = g.requiredObls(qseqcasePS)
  val qseqsubPS = qseqsubobs.toSeq.map { case (obl, info) =>
      g.applyTactic(obl, Solve[VeritasConstruct, VeritasFormula])
  }

  // TODO apply CaseDistinction to qcond case
  val qcondObl = rootInduction.selectCase(casenames(3), rootsubobs)
  val qcondPS = g.applyTactic(qcondObl, Solve[VeritasConstruct, VeritasFormula])

  //apply simply Solve-tactic to qgroup base case
  val qgroupObl = rootInduction.selectCase(casenames(4), rootsubobs)
  val qgroupPS = g.applyTactic(qgroupObl, Solve[VeritasConstruct, VeritasFormula])

  def attachSolveSteps(): Unit = {
    val oblsWithNoPS = g.obligationDFS().filter { obl => g.appliedStep(obl).isEmpty }
    val solvePS = oblsWithNoPS.map {obl =>
      g.applyTactic(obl, Solve[VeritasConstruct, VeritasFormula])
    }
  }
  //verify chosen steps with chosen verifiers
  def verifySingleStepsSimple() = {
    val simpleVampire4_1 = new TPTPVampireVerifier(5)
    val simpleVampire4_1_20 = new TPTPVampireVerifier(20)
    val simpleVampire4_1_120 = new TPTPVampireVerifier(120)

    // println(g.verifyProofStep(qvarPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(rootinductionPS, simpleVampire4_1_120).status)

    // //verify case distinction steps
    // println(g.verifyProofStep(qemptyPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(qgroupPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(qseqcasePS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(qsinglePS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(qcondPS, simpleVampire4_1_120).status)
    // g.verifyProofStep(progressLookupAnsMapInductionPS, simpleVampire4_1_120)
    //progressLookupAnsMapInductionCasesPS.foreach { ps =>
    //   println(g.verifyProofStep(ps, simpleVampire4_1_120).status)
    // }

    // // println(g.verifyProofStep(askPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(progressLookupQMapInductionPS, simpleVampire4_1_120).status)
    // progressLookupQMapInductionCasesPS.foreach { ps =>
    //    println(g.verifyProofStep(ps, simpleVampire4_1_20).status)
    // }
    // println(g.verifyProofStep(defquestionPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(questionPS, simpleVampire4_1_120).status)

    // println(g.verifyProofStep(expIsValueTruePS, simpleVampire4_1_120).status)
    println(g.verifyProofStep(expIsValueFalsePS, simpleVampire4_1_120).status)

    // println(g.verifyProofStep(progressReduceExpPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(constantProgressReduceExpPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(binopProgressReduceExpPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(binopProgressReduceExpSomeExpPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(binopProgressReduceExpNoExpPS, simpleVampire4_1_120).status)
    // println(g.verifyProofStep(unopProgressReduceExpDisitinctionPS, simpleVampire4_1_120).status)
    // unopProgressReduceExpCasesPS.foreach { ps =>
    //   println(g.verifyProofStep(ps, simpleVampire4_1_120).status)
    // }

    // qseqsubPS.foreach { ps =>
    //   println(g.verifyProofStep(ps, simpleVampire4_1_120).status)
    // }
  }
}

object QLSoundnessProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[VeritasConstruct, VeritasFormula]) = {
    PropertyTypes.registerWrapperType(g.store)
  }
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

  pg.verifySingleStepsSimple()

  val rootobl = pg.g.findObligation("QL progress").get
  val subobs = pg.g.requiredObls(pg.rootinductionPS)

  println("Root obligation: " + rootobl)
  println("Induction cases:")
  println(subobs)

}
