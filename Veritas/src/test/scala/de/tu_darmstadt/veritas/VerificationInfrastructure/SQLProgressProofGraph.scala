package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite
import quiver.{LEdge, LNode}

/**
  * Created by sylvia on 28/02/2017.
  */
class SQLProgressProofGraph extends FunSuite {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.testexamples.SQLDefs._

  // We instantiate S = Seq[VeritasConstruct] and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).

  type VeriProofNode = ProofNode[Seq[VeritasConstruct], VeritasConstruct]
  type VeriProofGraph = ProofGraph[Seq[VeritasConstruct], VeritasConstruct]
  type VeriVerificationStrategy = VerificationStrategy[Seq[VeritasConstruct], VeritasConstruct]

  val VeriSolve = Solve[Seq[VeritasConstruct], VeritasConstruct]()

  val fulltestspec: Seq[VeritasConstruct] = Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ TypeSystemInv.defs ++ SoundnessAuxDefs.defs

  def makeProofNode(nodename: String, tspec: Seq[VeritasConstruct], goal: VeritasConstruct,
                    strategy: VeriVerificationStrategy = VeriSolve): VeriProofNode =
    LNode(nodename, ProofStep[Seq[VeritasConstruct], VeritasConstruct](tspec, goal, strategy))

  def makeSingleNodeProofGraph(nodename: String, tspec: Seq[VeritasConstruct], goal: VeritasConstruct,
                               strategy: VeriVerificationStrategy = VeriSolve): VeriProofGraph = {
    ProofGraph(Seq(makeProofNode(nodename, tspec, goal, strategy)))
  }

  val progressroot = makeProofNode("SQL-progress", fulltestspec, SQLProgress,
    StructuralInduction[Seq[VeritasConstruct], VeritasConstruct](Seq(MetaVar("q"))))

  val tvaluecase = makeProofNode("SQL-progress-tvalue", fulltestspec, SQLProgressTtvalue)
  val tvalueedge : VerificationEdge = LEdge(progressroot.vertex, tvaluecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTtvalue.goals.head.name, Seq()))

  val selectfromwherecase = makeProofNode("SQL-progress-selectFromWhere", fulltestspec, SQLProgressTselectFromWhere)
  val selectFromWhereedge : VerificationEdge = LEdge(progressroot.vertex, selectfromwherecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name, Seq()))

  val unioncase = makeProofNode("SQL-progress-union", fulltestspec, localblockunion)
  val unionedge : VerificationEdge = LEdge(progressroot.vertex, unioncase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTUnion.goals.head.name,
      Seq(SQLProgressTUnionIH1, SQLProgressTUnionIH2)))

  val intersectioncase = makeProofNode("SQL-progress-intersection", fulltestspec, localblockintersection)
  val intersectionedge : VerificationEdge = LEdge(progressroot.vertex, intersectioncase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTIntersection.goals.head.name,
      Seq(SQLProgressTIntersectionIH1, SQLProgressTIntersectionIH2)))

  val differencecase = makeProofNode("SQL-progress-difference", fulltestspec, localblockdifference)
  val differenceedge : VerificationEdge = LEdge(progressroot.vertex, differencecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTDifference.goals.head.name,
      Seq(SQLProgressTDifferenceIH1, SQLProgressTDifferenceIH2)))

  val SQLbasicproofgraph: VeriProofGraph = ProofGraph(
    Seq(progressroot, selectfromwherecase, unioncase, intersectioncase, differencecase),
    Seq(tvalueedge, selectFromWhereedge, unionedge, intersectionedge, differenceedge))




}
