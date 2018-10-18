package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{CaseDistinction, Solve, StructuralInduction}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.inputdsl.{FunctionDSL, SymTreeDSL}
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.translator.{FunctionExpressionTranslator, ScalaSPLTranslator}
import de.tu_darmstadt.veritas.scalaspl.util.ScalaMetaUtils


class AESoundnessProofGraph(storefile: File) {


  val sourcefile = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala")
  val fullAESpec: Module = new ScalaSPLTranslator().translate(sourcefile)

  //collect domain-specific knowledge
  val builder = DomainSpecificKnowledgeBuilder()
  val dsk = builder.build(sourcefile)

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](storefile) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]

  // register all types in graph once to Xodus library
  PropertyTypes.registerWrapperType(g.store)

  val specenq = new VeritasSpecEnquirer(fullAESpec)

  //handle for simpler access to proof graph parts
  val PG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](g, ProofGraphUI.extractGoalOrLemmaName)

  def addSolveTacticToAllLeaves(): Unit = {
    val leaves = g.leaves()
    for (o <- leaves) {
      g.applyTactic(o, Solve[VeritasConstruct, VeritasFormula])
    }
  }

  //obtain translated progress root obligation from built domain-specific knowledge
  val progress_tr: TypingRule = dsk.lookupByFunName(dsk.progressProperties, "reduce").head
  val progressObligation: g.Obligation = g.newObligation(fullAESpec, Goals(Seq(progress_tr), None))
  g.storeObligation("Progress", progressObligation)

  //apply structural induction on a given induction var to a given obligation and retrieve all resulting obligations
  def applyInductionGetCases(obl: g.Obligation, indvar: MetaVar): Map[String, (g.Obligation, EdgeLabel)] = {
    val indtac = StructuralInduction(indvar, fullAESpec, specenq)
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

  //apply top-level induction
  val rootobl = g.findObligation("Progress").get
  val rootobl_edge_map = applyInductionGetCases(rootobl, MetaVar("t1"))

  val steps = applySolveToAllSub(rootobl)

  val simpleVampire4_1 = new TPTPVampireVerifier(5)
  val simpleVampire4_1_20 = new TPTPVampireVerifier(20)
  val simpleVampire4_1_120 = new TPTPVampireVerifier(120)

  val smtlibVampire = new SMTLibVerifier {
    override def prover: Prover[SMTLibFormat] = VampireZ3(5)

    /** Textual description that should be unique (used for ordering verifiers) */
    override val desc: String = "SMTLib-Vampire verifier"
  }

  val trivial_obls = Seq(rootobl_edge_map("ProgressTrue"), rootobl_edge_map("ProgressFalse"), rootobl_edge_map("ProgressZero")) map (_._1)
  val stepresultstrivial = for (obl <- trivial_obls) yield {
    val step = g.appliedStep(obl).get
    g.verifyProofStep(step, simpleVampire4_1)
  }

  val succ_obl = rootobl_edge_map("ProgressSucc")._1
  val succstep = g.appliedStep(succ_obl).get
  val stepresultsucc = g.verifyProofStep(succstep, simpleVampire4_1, Some("AE-Inconclusive/Succ"))

  val stepresults = stepresultstrivial :+ stepresultsucc

  // print some results for debugging
  for (sr <- stepresults) sr.status match {
    case Finished(Proved(ATPResultDetails(_,_, Some(proof), Some(lemmas),_)), _) =>
      println("Proof: " +  proof)
      println("Used lemmas: " + lemmas)
    case s => println(s)
  }



  //retrieve Ifelse-case
  val ifcase_obl = rootobl_edge_map("ProgressIfelse")._1

  //code below does not compile like this, work in progress
  // pass metavar names
  val funExpTranslator = new FunctionExpressionTranslator(Seq("vTerm0"))

  def translateFunExp(str: String): TypingRuleJudgment = {
    val term = ScalaMetaUtils.getTerm(str)
    val exp = funExpTranslator.translateExp(term)
    FunctionExpJudgment(exp)
  }


  val ifcasetactic = CaseDistinction[VeritasConstruct, VeritasFormula](Map(
    "Truecase" -> Seq[TypingRuleJudgment](translateFunExp("vTerm0 == true")),
    "Falsecase" -> Seq[TypingRuleJudgment](translateFunExp("vTerm0 == false")),
    "Othercase" -> Seq[TypingRuleJudgment](translateFunExp("(vTerm0 != true) && (vTerm0 != false)"))), fullAESpec, specenq)
  val ifcasedistinction = g.applyTactic(ifcase_obl, ifcasetactic)


  def checkConsistency(): Unit = {
    val vampireProver = VampireTPTP("4.1", 120)
    val checker = ConsistencyChecker(VeritasTransformerBestStrat.config, vampireProver)
    println(checker.consistent(fullAESpec))
  }

}

object ConstructAESoundnessGraph extends App {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val file = new File("AESoundnessProofGraph-store")
  recursivedelete(file) //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for AESoundnessProofGraph-store.")

  val pg = new AESoundnessProofGraph(file)
  val PG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](pg.g, ProofGraphUI.extractGoalOrLemmaName)


  //print progress obligation in ScalaSPL (using ScalaSPL pretty printer)
  val prettyPrinter = new SimpleToScalaSPLSpecificationPrinter {
    override val printer: PrettyPrintWriter = new PrettyPrintWriter(new PrintWriter(System.out))
  }

  //pg.checkConsistency() //nothing found with timeout 120 sec

  prettyPrinter.printTypingRule(pg.progress_tr)

  val ifcase_tr: TypingRule = pg.ifcase_obl.goal match {
    case Goals(Seq(tr),_) => tr
    case t@TypingRule(_, _, _ ) => t
  }


  prettyPrinter.printTypingRule(ifcase_tr)
  //val ifelseobl_ui = PG_UI.getObligation("ProgressIfelse")
  //println(PG_UI.getAssembledProblem(ifelseobl_ui, VeritasTransformerBestStrat))
  //since we call pretty printing only for a typing rule and not for an entire model, we need to close the writer
  // manually in order to see the remaining parts of the typing rule printed
  prettyPrinter.printer.close()

  //visualize proof graph
  def visualizeGraph(filename: String) {
    val graphfile = new File(filename)
    if (file.exists()) recursivedelete(file)
    Dot(pg.g, graphfile)
  }

  visualizeGraph("ae_soundness.png")



}
