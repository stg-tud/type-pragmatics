package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.ConstructAESoundnessGraph.{file, pg, recursivedelete}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Solve, StructuralInduction}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{ATPResultDetails, Finished, Proved, TPTPVampireVerifier}
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

class ToyTest extends FunSuite {
  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  test("Explore specification") {
    import de.tu_darmstadt.veritas.scalaspl.ToySpec._

    def makeNat(n: Int): Nat = {
      if(n <= 0)
        zero()
      else
        succ(makeNat(n - 1))
    }

    def en(n: Int) = EValue(VNat(makeNat(n)))

    val e = EGreaterThan(
      EPlus(
        en(4),
        en(5)
      ),
      //evalue(vbool(True()))
      EPlus(
        en(1),
        en(3)
      )
    )

    var ee: Expr = e
    var running = true
    while(running) {
      val someEE = reduce(ee)
      someEE match {
        case someExpr(result) => ee = result
        case noExpr() => running = false
      }
      println(someEE)
    }
  }

  val specFile = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/ToySpec.scala")
  val spec: Module = new ScalaSPLTranslator().translate(specFile)
  val dsk = DomainSpecificKnowledgeBuilder().build(specFile)


  test("Construct proof graph") {
    val file = new File("ToySoundnessProof-allsteps")
    if (file.exists()) recursivedelete(file)
    if (!file.mkdir()) sys.error("Could not create new store for Toys Soundness proof.")

    val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
      new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
    PropertyTypes.registerWrapperType(g.store)

    val specenq = new VeritasSpecEnquirer(spec)

    //add progress root obligation
    val progressRule: TypingRule = dsk.lookupByFunName(dsk.progressProperties, "reduce").head
    val progressObligation: g.Obligation = g.newObligation(spec, Goals(Seq(progressRule), None))

    g.storeObligation("Progress", progressObligation)
    val rootobl = g.findObligation("Progress").get

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

    //apply structural induction on a given induction var to a given obligation and retrieve all resulting obligations
    def applyInductionGetCases(obl: g.Obligation, indvar: MetaVar): Map[String, (g.Obligation, EdgeLabel)] = {
      val indtac = StructuralInduction(indvar, spec, specenq)
      val ps = g.applyTactic(obl, indtac)
      val subobls = g.requiredObls(ps)
      indtac.enumerateCases(subobls)
    }

    def addSolveTacticToAllLeaves(): Unit = {
      val leaves = g.leaves()
      for (o <- leaves) {
        g.applyTactic(o, Solve[VeritasConstruct, VeritasFormula])
      }
    }

    val rootobl_edge_map = applyInductionGetCases(rootobl, MetaVar("e1"))

    val steps = applySolveToAllSub(rootobl)
    //addSolveTacticToAllLeaves()

    val simpleVampire4_1 = new TPTPVampireVerifier(5)
    val simpleVampire4_1_20 = new TPTPVampireVerifier(20)
    val simpleVampire4_1_120 = new TPTPVampireVerifier(120)

    val trivial_obls = Seq("ProgressEValue", "ProgressEPlus", "ProgressEGreaterThan")
      .map(rootobl_edge_map(_)._1)
    //val trivial_obls = Seq(rootobl)
    val stepresultstrivial = for (obl <- trivial_obls) yield {
      val step = g.appliedStep(obl).get
      g.verifyProofStep(step, simpleVampire4_1_20)
    }

    //visualize proof graph
    def visualizeGraph(filename: String) {
      val graphfile = new File(filename)
      if (file.exists()) recursivedelete(file)
      Dot(g, graphfile, "pdf")
    }

    val stepresults = stepresultstrivial //:+ stepresultsucc

    // print some results for debugging
    for (sr <- stepresults) sr.status match {
      case Finished(Proved(ATPResultDetails(_,_, Some(proof), Some(lemmas),_)), _) =>
        println("Proof: " +  proof)
        println("Used lemmas: " + lemmas)
      case s => println(s)
    }

    visualizeGraph("toy_soundness.pdf")
  }
}
