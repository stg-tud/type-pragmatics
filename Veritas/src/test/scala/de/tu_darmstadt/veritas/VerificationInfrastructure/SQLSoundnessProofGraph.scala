package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.TPTPVampireVerifier
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.inputdsl.{FunctionDSL, SymTreeDSL}


// Constructing the SQL soundness proof graph
// We instantiate S = VeritasConstruct (should be a Module) and P = VeritasConstruct (Should be Goal/local)
class SQLSoundnessProofGraph(file: File) {

  import SQLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
  SQLSoundnessProofGraph.initializeGraphTypes(g)

  val specenq = new VeritasSpecEnquirer(fullSQLspec)


  //add progress root obligation
  val progressObligation: g.Obligation = g.newObligation(fullSQLspec, SQLProgress)
  g.storeObligation("SQL progress", progressObligation)

  //apply structural induction on a given induction var to a given obligation and retrieve all resulting obligations
  def applyInductionGetCases(obl: g.Obligation, indvar: MetaVar): Map[String, (g.Obligation, EdgeLabel)] = {
    val indtac = StructuralInduction(indvar, fullSQLspec, specenq)
    val ps = g.applyTactic(obl, indtac)
    val subobls = g.requiredObls(ps)
    indtac.enumerateCases(subobls)
  }

  // mutable function: apply Solve tactic to all sub-obligations of a given obligation and return the resulting proof steps
  def applySolveToAllSub(obl: g.Obligation): Seq[g.ProofStep] = {
    g.appliedStep(obl) match {
      case Some(ps) => {
        val subobls = g.requiredObls(ps).map(_._1)
        (for (o <- subobls) yield g.applyTactic(o, Solve[VeritasConstruct, VeritasFormula])).toSeq
      }
      case None => Seq()
    }

  }

  val rootobl = g.findObligation("SQL progress").get
  val rootobl_edge_map = applyInductionGetCases(rootobl, MetaVar("q"))

  val rootcasenames = rootobl_edge_map.keys.toSeq.sortWith(_ > _)

  //apply simply Solve-tactic to t-value base case
  val tvaluecaseobl = rootobl_edge_map(rootcasenames(0))._1
  val tvaluecasePS = g.applyTactic(tvaluecaseobl, Solve[VeritasConstruct, VeritasFormula])

  // Case distinctions for Union, Intersection, Difference cases
  // construct the case predicates:

  def makeSetCasePreds(fv1name: String, fv2name: String): Map[String, Seq[TypingRuleJudgment]] = {
    import FunctionDSL._
    import SymTreeDSL._
    import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._

    //TODO: here t1 and t2 are guessed to be free variable names (which will work out in this particular case) - better: generate fresh names
    val case1pred: Seq[TypingRuleJudgment] = (~(Symbol(fv1name)) === 'tvalue (~'t1)) & (~(Symbol(fv2name)) === 'tvalue (~'t2))
    val case2pred: Seq[TypingRuleJudgment] = (~(Symbol(fv1name)) === 'tvalue (~'t1)) & (forall(~'t2) | (~(Symbol(fv2name)) ~= 'tvalue (~'t2)))
    val case3pred: Seq[TypingRuleJudgment] = Seq(forall(~'t1) | (~(Symbol(fv1name)) ~= 'tvalue (~'t1)))

    Map(("Both-Tvalues" -> case1pred),
      ("First-Tvalue-Second-not-Tvalue" -> case2pred),
      ("First-not-Tvalue" -> case3pred))
  }

  def makeSetCaseDistinction(fv1name: String, fv2name: String): CaseDistinction[VeritasConstruct, VeritasFormula] =
    CaseDistinction(makeSetCasePreds(fv1name, fv2name), specenq)

  def extract2FVNames(casename: String): (String, String) = {
    val cases = rootobl_edge_map(casename)._2
    val (fv1, fv2) = cases match {
      case StructInductCase(_, fv, _, _) => (fv(0).fixedvar, fv(1).fixedvar)
      case _ => sys.error(s"Expected a StructInductCase label at the given casename $casename")
    }

    (fv1, fv2) match {
      case (MetaVar(n1), MetaVar(n2)) => (n1, n2)
      case (FunctionMeta(MetaVar(n1)), FunctionMeta(MetaVar(n2))) => (n1, n2)
      case _ => sys.error("fixed variables did not have expected format")
    }
  }


  //val unionCaseDistinction = SetCaseDistinction(unionsym, sunion) //hard-coded mock tactic
  val unioncaseFVs = extract2FVNames(rootcasenames(2))
  val unionCaseDistinction = makeSetCaseDistinction(unioncaseFVs._1, unioncaseFVs._2)

  val unioncaseobl = rootobl_edge_map(rootcasenames(2))._1
  val unioncasePS = g.applyTactic(unioncaseobl, unionCaseDistinction)

  //val intersectionCaseDistinction = SetCaseDistinction(intersectionsym, sintersection) //hard-coded mock tactic
  val intersectioncaseFVs = extract2FVNames(rootcasenames(3))
  val intersectionCaseDistinction = makeSetCaseDistinction(intersectioncaseFVs._1, intersectioncaseFVs._2)

  val intersectioncaseobl = rootobl_edge_map(rootcasenames(3))._1
  val intersectioncasePS = g.applyTactic(intersectioncaseobl, intersectionCaseDistinction)

  //val differenceCaseDistinction = SetCaseDistinction(differencesym, sdifference) //hard-coded mock tactic
  val differencecaseFVs = extract2FVNames(rootcasenames(4))
  val differenceCaseDistinction = makeSetCaseDistinction(differencecaseFVs._1, differencecaseFVs._2)

  val differencecaseobl = rootobl_edge_map(rootcasenames(4))._1
  val differencecasePS = g.applyTactic(differencecaseobl, differenceCaseDistinction)

  //apply Solve tactic to all of the set cases
  val setPS = (for (c <- Seq(unioncaseobl, intersectioncaseobl, differencecaseobl)) yield {
    applySolveToAllSub(c)
  }).flatten


  //prove selectFromWhereCase via auxiliary lemmas:
  val selcase = rootobl_edge_map(rootcasenames(1))._1


  //apply lemma application tactic to selection case
  val selLemmaTac = LemmaApplication(Seq(successfulLookup, welltypedLookup,
    filterPreservesType, projectTableProgress), fullSQLspec, specenq)
  val selLemmaPS = g.applyTactic(selcase, selLemmaTac)


  // prove lemma successfulLookup via simple structural induction, cases via Solve
  val successfulLookupobl = selLemmaTac.selectLemma(successfulLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))
  applyInductionGetCases(successfulLookupobl, MetaVar("TS"))
  val successfulLookup_cases_PS = applySolveToAllSub(successfulLookupobl)


  // prove lemma welltypedLookup via simple structural induction
  val welltypedLookupobl = selLemmaTac.selectLemma(welltypedLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))
  applyInductionGetCases(welltypedLookupobl, MetaVar("TS"))
  val welltypedLookup_cases_PS = applySolveToAllSub(welltypedLookupobl)


  // prove lemma filterPreservesType via auxiliary lemma filterRowsPreservesTable
  val filterPreservesTypeobl = selLemmaTac.selectLemma(filterPreservesType.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val filterPreservesTypeTac = LemmaApplication(Seq(filterRowsPreservesTable), fullSQLspec, specenq)
  val filterPreservesTypePS = g.applyTactic(filterPreservesTypeobl, filterPreservesTypeTac)

  val filterRowsPreservesTableObl = filterPreservesTypeTac.selectLemma(filterRowsPreservesTable.lemmas.head.name,
    g.requiredObls(filterPreservesTypePS))

  // prove lemmma filterRowsPreservesTable via simple structural induction
  applyInductionGetCases(filterRowsPreservesTableObl, MetaVar("rt"))
  val filterRowsPreservesTablePS = applySolveToAllSub(filterRowsPreservesTableObl)

  //try to prove projectTableProgress via lemma application with projectColsProgress?
  //yes, works, apparently no case distinction necessary!
  val projectTableProgressobl = selLemmaTac.selectLemma(projectTableProgress.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  private val projectTableProgressTac = LemmaApplication(Seq(projectColsProgress), fullSQLspec, specenq)
  val projectTableProgressPS = g.applyTactic(projectTableProgressobl, projectTableProgressTac)

  //prove projectColsProgress via induction
  val projectColsProgressObl = projectTableProgressTac.selectLemma(projectColsProgress.lemmas.head.name,
    g.requiredObls(projectTableProgressPS))

  val projectColsProgress_casemap = applyInductionGetCases(projectColsProgressObl, MetaVar("al2"))
  val projectColsProgress_casenames = projectColsProgress_casemap.keys.toSeq.sortWith(_ < _)

  val projectColsProgressbasecase = projectColsProgress_casemap(projectColsProgress_casenames(0))._1

  val projectColsProgressbasecasePS = g.applyTactic(projectColsProgressbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  // step case requires an auxiliary lemma (projectTypeImpliesFindCol)
  val projectColsProgressstepcase = projectColsProgress_casemap(projectColsProgress_casenames(1))._1
  private val projectColsProgressStepCaseTac = LemmaApplication(Seq(projectTypeImpliesFindCol), fullSQLspec, specenq)
  val projectColsProgressstepcasePS = g.applyTactic(projectColsProgressstepcase, projectColsProgressStepCaseTac)

  //prove projectTypeImpliesFindCol via induction
  val projectTypeImpliesFindColObl = projectColsProgressStepCaseTac.selectLemma(projectTypeImpliesFindCol.lemmas.head.name,
    g.requiredObls(projectColsProgressstepcasePS))

  val projectTypeImpliesFindCol_casemap = applyInductionGetCases(projectTypeImpliesFindColObl, MetaVar("al2"))
  val projectTypeImpliesFindCol_casenames = projectTypeImpliesFindCol_casemap.keys.toSeq.sortWith(_ < _)

  val projectTypeImpliesFindColbasecase = projectTypeImpliesFindCol_casemap(projectTypeImpliesFindCol_casenames(0))._1
  val projectTypeImpliesFindColbasecasePS = g.applyTactic(projectTypeImpliesFindColbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  // step case requires two auxiliary lemmas
  val projectTypeImpliesFindColstepcase = projectTypeImpliesFindCol_casemap(projectTypeImpliesFindCol_casenames(1))._1
  private val projectTypeImpliesFindColStepCaseTac = LemmaApplication(Seq(findColTypeImpliesfindCol, projectTypeAttrLImpliesfindAllColType), fullSQLspec, specenq)
  val projectTypeImpliesFindColstepcasePS = g.applyTactic(projectTypeImpliesFindColstepcase, projectTypeImpliesFindColStepCaseTac)

  //prove findColTypeImpliesfindCol via induction
  val findColTypeImpliesfindColObl = projectTypeImpliesFindColStepCaseTac.selectLemma(findColTypeImpliesfindCol.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  val findColTypeImpliesfindCol_casemap = applyInductionGetCases(findColTypeImpliesfindColObl, MetaVar("al"))
  val findColTypeImpliesfindCol_casenames = findColTypeImpliesfindCol_casemap.keys.toSeq.sortWith(_ < _)

  val findColTypeImpliesfindColbasecase = findColTypeImpliesfindCol_casemap(findColTypeImpliesfindCol_casenames(0))._1
  val findColTypeImpliesfindColbasecasePS = g.applyTactic(findColTypeImpliesfindColbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  //step requires auxiliary lemma dropFirstColRawPreservesWelltypedRaw
  val findColTypeImpliesfindColstepcase = findColTypeImpliesfindCol_casemap(findColTypeImpliesfindCol_casenames(1))._1
  private val findColTypeImpliesfindColStepcaseTac = LemmaApplication(Seq(dropFirstColRawPreservesWelltypedRaw), fullSQLspec, specenq)
  val findColTypeImpliesfindColstepcasePS = g.applyTactic(findColTypeImpliesfindColstepcase, findColTypeImpliesfindColStepcaseTac)

  //prove projectTypeAttrLImpliesfindAllColType via induction
  val projectTypeAttrLImpliesfindAllColTypeObl = projectTypeImpliesFindColStepCaseTac.selectLemma(projectTypeAttrLImpliesfindAllColType.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  applyInductionGetCases(projectTypeAttrLImpliesfindAllColTypeObl, MetaVar("al"))
  val projectTypeAttrLImpliesfindAllColTypePS = applySolveToAllSub(projectTypeAttrLImpliesfindAllColTypeObl)

  //prove dropFirstColRawPreservesWelltypedRaw via induction
  val dropFirstColRawPreservesWelltypedRawObl = findColTypeImpliesfindColStepcaseTac.selectLemma(dropFirstColRawPreservesWelltypedRaw.lemmas.head.name,
    g.requiredObls(findColTypeImpliesfindColstepcasePS))

  applyInductionGetCases(dropFirstColRawPreservesWelltypedRawObl, MetaVar("rt"))
  val dropFirstColRawPreservesWelltypedRawPS = applySolveToAllSub(dropFirstColRawPreservesWelltypedRawObl)

  //verify chosen steps with chosen verifiers
  def verifySingleStepsSimple() = {
    val simpleVampire4_1 = new TPTPVampireVerifier(5)
    val simpleVampire4_1_20 = new TPTPVampireVerifier(20)

    g.verifyProofStep(tvaluecasePS, simpleVampire4_1)

    //verify case distinction steps
    g.verifyProofStep(unioncasePS, simpleVampire4_1)
    g.verifyProofStep(intersectioncasePS, simpleVampire4_1)
    g.verifyProofStep(differencecasePS, simpleVampire4_1)


    //verify the individual set cases (inconclusive)
    for (ps <- setPS) {
      g.verifyProofStep(ps, simpleVampire4_1)
    }

    //Inconclusive step
    g.verifyProofStep(selLemmaPS, simpleVampire4_1)

    //successful steps (?)
    for (ps <- successfulLookup_cases_PS)
      g.verifyProofStep(ps, simpleVampire4_1)

    for (ps <- welltypedLookup_cases_PS)
      g.verifyProofStep(ps, simpleVampire4_1)


    g.verifyProofStep(filterPreservesTypePS, simpleVampire4_1)

    for (ps <- filterRowsPreservesTablePS)
        g.verifyProofStep(ps, simpleVampire4_1)

    g.verifyProofStep(projectTableProgressPS, simpleVampire4_1_20)

    g.verifyProofStep(projectColsProgressbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectColsProgressstepcasePS, simpleVampire4_1)

    g.verifyProofStep(projectTypeImpliesFindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectTypeImpliesFindColstepcasePS, simpleVampire4_1)

    g.verifyProofStep(findColTypeImpliesfindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(findColTypeImpliesfindColstepcasePS, simpleVampire4_1)

    for (ps <- projectTypeAttrLImpliesfindAllColTypePS)
      g.verifyProofStep(ps, simpleVampire4_1)

    for (ps <- dropFirstColRawPreservesWelltypedRawPS)
      g.verifyProofStep(ps, simpleVampire4_1)

  }


}

object SQLSoundnessProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[VeritasConstruct, VeritasFormula]) = {
    PropertyTypes.registerWrapperType(g.store)
    //register all the necessary property types
    //PropertyTypes.registerPropertyType[VeritasConstruct](g.store)
    //PropertyTypes.registerPropertyType[VeritasFormula](g.store)
    //PropertyTypes.registerPropertyType[VeritasFormula with Ordered[VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[Module](g.store)
    //PropertyTypes.registerPropertyType[Goals](g.store)
    //PropertyTypes.registerPropertyType[rootInductionProgress.type](g.store)
    //PropertyTypes.registerPropertyType[StructInductCase[VeritasConstruct, VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[SetCaseDistinction](g.store)
    //PropertyTypes.registerPropertyType[CaseDistinctionCase[VeritasConstruct, VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[Finished[_, _]](g.store)
    //PropertyTypes.registerPropertyType[VerifierFailure[_, _]](g.store)
    //PropertyTypes.registerPropertyType[TSTPProof](g.store)
    //PropertyTypes.registerPropertyType[Solve[_, _]](g.store)
    //PropertyTypes.registerPropertyType[MockLemmaApplication](g.store)
    //PropertyTypes.registerPropertyType[LemmaApplicationStep[_]](g.store)
    //PropertyTypes.registerPropertyType[successfulLookupInduction.type](g.store)
    //PropertyTypes.registerPropertyType[welltypedLookupInduction.type](g.store)
    //PropertyTypes.registerPropertyType[filterRowsPreservesTableInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectColsProgressInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectTypeImpliesFindColInduction.type](g.store)
    //PropertyTypes.registerPropertyType[findColTypeImpliesfindColInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectTypeAttrLImpliesfindAllColTypeInduction.type](g.store)
    //PropertyTypes.registerPropertyType[dropFirstColRawPreservesWelltypedRawInduction.type](g.store)
    //PropertyTypes.registerPropertyType[verifier.Unknown[_, _]](g.store)
  }
}


// Executing this object creates a new SQL Soundness Proof Graph,
// attempting to verify as much as possible
object ConstructSQLSoundnessGraph extends App {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val file = new File("SQLSoundnessProofGraph-store")
  recursivedelete(file) //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-store.")

  val pg = new SQLSoundnessProofGraph(file)

  pg.verifySingleStepsSimple()


}

