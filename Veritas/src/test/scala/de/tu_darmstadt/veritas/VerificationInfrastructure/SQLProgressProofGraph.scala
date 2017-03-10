package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

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

  // We instantiate S = Spec and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).

  type VeriProofNode = ProofNode[Spec, VeritasConstruct]
  type VeriProofGraph = ProofGraphQuiver[Spec, VeritasConstruct]
  type VeriProofGraphX = ProofGraphXodus[Spec, VeritasConstruct]
  type VeriVerificationStrategy = VerificationStrategy[Spec, VeritasConstruct]

  case class Spec(content: Seq[VeritasConstruct]) extends Ordered[Spec] {
    val ord = Ordering.Iterable[VeritasConstruct](Ordering.ordered[VeritasConstruct](x => x))
    override def compare(that: Spec): Int = ord.compare(this.content, that.content)
  }

  // special purpose object for S = Spec and P = VeritasConstruct
  object ProofGraphXodus {
    private def registerAllPropTypes(g: VeriProofGraphX): Unit = {
      //TODO: find a better way of how to add all necessary properties? Is there a generic way to do that?

      PropertyTypes.registerPropertyType[Goals]
      PropertyTypes.registerPropertyType[Local]
      PropertyTypes.registerPropertyType[StructuralInduction[Spec, VeritasConstruct]]
      PropertyTypes.registerPropertyType[Spec]
      PropertyTypes.registerPropertyType[Solve[Spec, VeritasConstruct]]
      PropertyTypes.registerPropertyType[StructInductCase[Spec]]
    }


    def apply(nodelist: Seq[VeriProofNode], edgelist: Seq[VerificationEdge],
              storename: String = "temporary-store"): VeriProofGraphX = {
      //TODO: better management of stores needed?
      val file = File.createTempFile(storename, "")
      file.delete()
      file.mkdir()

      val g: VeriProofGraphX = new ProofGraphXodus[Spec, VeritasConstruct](file)
      registerAllPropTypes(g)

      nodelist.foreach(g.addNode(_))
      edgelist.foreach(g.addEdge(_))

      g
    }
  }

  val VeriSolve = Solve[Spec, VeritasConstruct]()

  val fulltestspec: Spec = Spec(Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ TypeSystemInv.defs ++ SoundnessAuxDefs.defs)

  def makeProofNode(nodename: String, tspec: Spec, goal: VeritasConstruct,
                    strategy: VeriVerificationStrategy = VeriSolve): VeriProofNode =
    LNode(nodename, ProofStep[Spec, VeritasConstruct](tspec, goal, strategy))

  def makeSingleNodeProofGraph(nodename: String, tspec: Spec, goal: VeritasConstruct,
                               strategy: VeriVerificationStrategy = VeriSolve): VeriProofGraph = {
    ProofGraphQuiver(Seq(makeProofNode(nodename, tspec, goal, strategy)))
  }

  def makeSingleNodeProofGraphX(nodename: String, tspec: Spec, goal: VeritasConstruct,
                               strategy: VeriVerificationStrategy = VeriSolve): VeriProofGraphX = {
    ProofGraphXodus(Seq(makeProofNode(nodename, tspec, goal, strategy)), Seq())
  }

  val progressroot = makeProofNode("SQL-progress", fulltestspec, SQLProgress,
    StructuralInduction[Spec, VeritasConstruct](Spec(Seq(MetaVar("q")))))

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

  val SQLbasicproofgraph: VeriProofGraphX = ProofGraphXodus(
    Seq(progressroot, tvaluecase, selectfromwherecase, unioncase, intersectioncase, differencecase),
    Seq(tvalueedge, selectFromWhereedge, unionedge, intersectionedge, differenceedge))




  //case split for union case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTUnion1 = goal(
    ( ('q1 === 'tvalue(~'t1)) &
      ('q2 === 'tvalue(~'t2)) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion2 = goal(
    ( (~'q1 === 'tvalue(~'t1)) &
      (forall (~'t2) | ('q2 ~= 'tvalue(~'t2))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion3 = goal(
    ( (forall (~'t1) | ('q1 ~= 'tvalue(~'t1))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val localblockunioncase1 = local(unionconsts, SQLProgressTUnion1)
  val localblockunioncase2 = local(unionconsts, SQLProgressTUnionIH2, SQLProgressTUnion2)
  val localblockunioncase3 = local(unionconsts, SQLProgressTUnionIH1, SQLProgressTUnion3)


}
