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

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._

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
  val tvalueedge: VerificationEdge = LEdge(progressroot.vertex, tvaluecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTtvalue.goals.head.name, Seq()))

  val selectfromwherecase = makeProofNode("SQL-progress-selectFromWhere", fulltestspec, SQLProgressTselectFromWhere)
  val selectFromWhereedge: VerificationEdge = LEdge(progressroot.vertex, selectfromwherecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name, Seq()))

  val unioncase = makeProofNode("SQL-progress-union", fulltestspec, localblockunion)
  val unionedge: VerificationEdge = LEdge(progressroot.vertex, unioncase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTUnion.goals.head.name,
      Seq(SQLProgressTUnionIH1, SQLProgressTUnionIH2)))

  val intersectioncase = makeProofNode("SQL-progress-intersection", fulltestspec, localblockintersection)
  val intersectionedge: VerificationEdge = LEdge(progressroot.vertex, intersectioncase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTIntersection.goals.head.name,
      Seq(SQLProgressTIntersectionIH1, SQLProgressTIntersectionIH2)))

  val differencecase = makeProofNode("SQL-progress-difference", fulltestspec, localblockdifference)
  val differenceedge: VerificationEdge = LEdge(progressroot.vertex, differencecase.vertex,
    StructInductCase[VeritasConstruct](SQLProgressTDifference.goals.head.name,
      Seq(SQLProgressTDifferenceIH1, SQLProgressTDifferenceIH2)))

  val SQLbasicproofgraph: VeriProofGraph = ProofGraph(
    Seq(progressroot, selectfromwherecase, unioncase, intersectioncase, differencecase),
    Seq(tvalueedge, selectFromWhereedge, unionedge, intersectionedge, differenceedge))


  //case split for union case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTUnion1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val localblockunioncase1 = local(unionconsts, SQLProgressTUnion1)
  //TODO: How to deal with the local blocks here?
  val unioncase1node = makeProofNode("SQL-progress-union-1", fulltestspec, localblockunioncase1)
  //TODO: add edges

  val localblockunioncase2 = local(unionconsts, SQLProgressTUnionIH2, SQLProgressTUnion2)
  val unioncase2node = makeProofNode("SQL-progress-union-2", fulltestspec, localblockunioncase2)

  val localblockunioncase3 = local(unionconsts, SQLProgressTUnionIH1, SQLProgressTUnion3)
  val unioncase3node = makeProofNode("SQL-progress-union-3", fulltestspec, localblockunioncase3)


  //case split for intersection case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTIntersection1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTIntersection2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTIntersection3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val localblockintersectioncase1 = local(intersectionconsts, SQLProgressTIntersection1)
  val intersectioncase1node = makeProofNode("SQL-progress-intersection-1", fulltestspec, localblockintersectioncase1)

  val localblockintersectioncase2 = local(intersectionconsts, SQLProgressTIntersectionIH2, SQLProgressTIntersection2)
  val intersectioncase2node = makeProofNode("SQL-progress-intersection-2", fulltestspec, localblockintersectioncase2)

  val localblockintersectioncase3 = local(intersectionconsts, SQLProgressTIntersectionIH1, SQLProgressTIntersection3)
  val intersectioncase3node = makeProofNode("SQL-progress-intersection-3", fulltestspec, localblockintersectioncase3)


  //case split for difference case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTDifference1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTDifference2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTDifference3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val localblockdifferencecase1 = local(differenceconsts, SQLProgressTDifference1)
  val differencecase1node = makeProofNode("SQL-progress-difference-1", fulltestspec, localblockdifferencecase1)

  val localblockdifferencecase2 = local(differenceconsts, SQLProgressTDifferenceIH2, SQLProgressTDifference2)
  val differencecase2node = makeProofNode("SQL-progress-difference-2", fulltestspec, localblockdifferencecase2)

  val localblockdifferencecase3 = local(differenceconsts, SQLProgressTDifferenceIH1, SQLProgressTDifference3)
  val differencecase3node = makeProofNode("SQL-progress-difference-3", fulltestspec, localblockdifferencecase3)


  // here, the SQL lemmas necessary for progress (selectFromWhere case) start

  val successfullookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup")(
      exists(~'t) |
        ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)))
  )

  val welltypedlookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup")(
      'welltypedtable (~'tt, ~'t))
  )

  val filterpreservestype: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t) &
      (~'ft === 'filterTable (~'t, ~'p))
      ).===>("filter-preserves-type")(
      'welltypedtable (~'tt, ~'ft))
  )

  val projecttableprogress: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t) &
      ('projectType (~'s, ~'tt) === 'someTType (~'tt2))
      ).===>("projectTable-progress")(
      exists(~'t2) |
        'projectTable (~'s, ~'t) === 'someTable (~'t2))
  )


}
