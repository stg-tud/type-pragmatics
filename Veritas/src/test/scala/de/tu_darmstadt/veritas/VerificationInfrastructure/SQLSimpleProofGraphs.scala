package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Solve
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Finished
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

/**
  * Created by sylvia on 22/02/2017.
  */
class SQLSimpleProofGraphs extends FunSuite {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs.{Tables, TableAux, TStore, TContext, Syntax, Semantics, TypeSystem, TypeSystemInv, SoundnessAuxDefs}

  // We instantiate S = Seq[VeritasConstruct] and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).

  val testspec: Module = Module("SQLspec", Seq(), Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ SoundnessAuxDefs.defs)

  def makeSingleNodeProofGraph(nodename: String, tspec: VeritasConstruct, goal: VeritasConstruct):
      ProofGraphXodus[VeritasConstruct, VeritasConstruct] = {
    val file = File.createTempFile("veritas-xodus-test-store", "")
    file.delete()
    file.mkdir()
    val pg : ProofGraphXodus[VeritasConstruct, VeritasConstruct] = new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file)

    PropertyTypes.registerPropertyType[Module](pg.store)
    PropertyTypes.registerPropertyType[Local](pg.store)
    PropertyTypes.registerPropertyType[Finished[_, _]](pg.store)

    val obl = pg.obligationProducer.newObligation(tspec, goal)
    pg.storeObligation(nodename, obl)
    pg.applyTactic(obl, Solve[VeritasConstruct, VeritasConstruct])

    pg
  }


  test("Verify ProofGraph for test-1 goal") {
    val conc = _toFunctionExpJudgment('filterSingleRow (~'pred, ~'al, ~'row))

    val test1goal: Goals = goal(
      ((~'al === 'acons ('a, 'acons ('b, 'acons ('c, 'aempty)))) &
        (~'pred === 'eq ('lookup ('b), 'constant ('gv))) &
        (~'row === 'rcons ('fv, 'rcons ('gv, 'rcons ('hv, 'rempty))))
        ).===>("test-1")
      (conc))

    val test1: Local = local(
      differentconsts('fv ::> 'Val,
        'gv ::> 'Val,
        'hv ::> 'Val,
        'a ::> 'Name,
        'b ::> 'Name,
        'c ::> 'Name),
      test1goal
    )

    val pg = makeSingleNodeProofGraph("test-1 goal", testspec, test1)

    val obl = pg.findObligation("test-1 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-2 goal") {
    val test2goal: Goals = goal(
      ((~'al === 'acons ('a, 'acons ('b, 'aempty))) &
        (~'table1 === 'table (~'al, 'tcons ('r1, 'tempty))) &
        (~'table2 === 'table (~'al, 'tcons ('r2, 'tempty)))
        ).===>("test-2")(
        'reduce ('Intersection ('tvalue (~'table1), 'tvalue (~'table2)), 'emptyStore) ===
          'someQuery ('tvalue ('table (~'al, 'tempty)))
      ))

    val test2: Local = local(
      differentconsts('r1 ::> 'Row,
        'r2 ::> 'Row,
        'a ::> 'Name,
        'b ::> 'Name),
      test2goal
    )

    val pg = makeSingleNodeProofGraph("test-2 goal", testspec, test2)

    val obl = pg.findObligation("test-2 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-3 goal") {
    val test3goal: Goals = goal(
      ((~'al === 'acons ('a1, 'acons ('a2, 'acons ('a3, 'aempty)))) &
        (~'row === 'rcons ('v1, 'rcons ('v2, 'rcons ('v3, 'rempty)))) &
        (~'rt === 'tcons (~'row, 'tempty))
        ).===>("test-3")(
        'findCol ('a2, ~'al, ~'rt) === 'someRawTable ('tcons ('rcons ('v2, 'rempty), 'tempty))
      ))

    val test3: Local = local(
      differentconsts('a1 ::> 'Name,
        'a2 ::> 'Name,
        'a3 ::> 'Name,
        'v1 ::> 'Val,
        'v2 ::> 'Val,
        'v3 ::> 'Val),
      test3goal
    )

    val pg = makeSingleNodeProofGraph("test-3 goal", testspec, test3)

    val obl = pg.findObligation("test-3 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-4 goal") {
    val test4goal: Goals = goal(
      ((~'table1 === 'tcons ('row1, 'tcons ('row2, 'tempty))) &
        (~'table2 === 'tcons ('row3, 'tcons ('row1, 'tempty)))
        ).===>("test-4")(
        'rawDifference (~'table1, ~'table2) === 'tcons ('row2, 'tempty)
      ))

    val test4: Local = local(
      differentconsts('row1 ::> 'Row,
        'row2 ::> 'Row,
        'row3 ::> 'Row),
      test4goal
    )

    val pg = makeSingleNodeProofGraph("test-4 goal", testspec, test4)

    val obl = pg.findObligation("test-4 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-5 goal") {
    val conc: FunctionExpJudgment = _toFunctionExpJudgment('rowIn ('row4, ~'table))

    val test5goal: Goals = goal(
      (~'table === 'tcons ('row1, 'tcons ('row2, 'tcons ('row3, 'tcons ('row4, 'tempty))))
        ).===>("test-5")(
        conc
      ))

    val test5: Local = local(
      differentconsts('row1 ::> 'Row,
        'row2 ::> 'Row,
        'row3 ::> 'Row,
        'row4 ::> 'Row),
      test5goal
    )

    val pg = makeSingleNodeProofGraph("test-5 goal", testspec, test5)

    val obl = pg.findObligation("test-5 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-6 goal") {

    val test6goal: Goals = goal(
      ((~'r1 === 'rcons ('v1, 'rempty)) &
        (~'r2 === 'rcons ('v2, 'rempty)) &
        (~'r3 === 'rcons ('v3, 'rempty)) &
        (~'rt1 === 'tcons (~'r1, 'tcons (~'r1, 'tcons (~'r3, 'tempty)))) &
        (~'rt2 === 'tcons (~'r2, 'tcons (~'r2, 'tcons (~'r3, 'tempty)))) &
        (~'table1 === 'table (~'al, ~'rt1)) &
        (~'table2 === 'table (~'al2, ~'rt2)) &
        (~'rres === 'tcons (~'r1, 'tcons (~'r1, 'tcons (~'r2, 'tcons (~'r2, 'tcons (~'r3, 'tempty))))))
        ).===>("test-6")(
        'reduce ('Union ('tvalue (~'table1), 'tvalue (~'table2)), 'emptyStore) ===
          'someQuery ('tvalue ('table (~'al, ~'rres)))
      ))

    val test6: Local = local(
      differentconsts('v1 ::> 'Val,
        'v2 ::> 'Val,
        'v3 ::> 'Val),
      test6goal
    )

    val pg = makeSingleNodeProofGraph("test-6 goal", testspec, test6)

    val obl = pg.findObligation("test-6 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)

  }

  test("Verify ProofGraph for test-7 goal") {

    val test7goal: Goals = goal(
      ((~'tt1 === 'ttcons ('a, 'ft1, 'ttcons ('b, 'ft2, 'ttempty))) &
        (~'TTC === 'bindContext ('tn1, ~'tt1, 'emptyContext)) &
        (~'al === 'acons ('b, 'aempty)) &
        (~'TT === 'ttcons ('b, 'ft2, 'ttempty))
        ).===>("test-7")(
        ~'TTC |- 'selectFromWhere ('some (~'al), 'tn1, 'ptrue ()) :: ~'TT
      ))

    val test7: Local = local(
      differentconsts('a ::> 'Name,
        'b ::> 'Name,
        'ft1 ::> 'FType,
        'ft2 ::> 'FType,
        'tn1 ::> 'Name),
      test7goal
    )

    val pg = makeSingleNodeProofGraph("test-7 goal", testspec, test7)

    val obl = pg.findObligation("test-7 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-8 goal") {

    val prems: Seq[TypingRuleJudgment] =
      _toFunctionExpJudgment('welltypedtable ('tt1, 't1)) &
        _toFunctionExpJudgment('welltypedtable ('tt2, 't2)) &
        _toFunctionExpJudgment('welltypedtable ('tt3, 't3))

    val conc: FunctionExpJudgment = _toFunctionExpJudgment('StoreContextConsistent (~'TS, ~'TTC))

    val test8goal: Goals = goal(
      (prems &
        (~'TS === 'bindStore ('tn1, 't1, 'bindStore ('tn2, 't2, 'bindStore ('tn3, 't3, 'emptyStore)))) &
        (~'TTC === 'bindContext ('tn1, 'tt1, 'bindContext ('tn2, 'tt2, 'bindContext ('tn3, 'tt3, 'emptyContext))))
        ).===>("test-8")(
        conc))

    val test8: Local = local(
      differentconsts('tn1 ::> 'Name,
        'tn2 ::> 'Name,
        'tn3 ::> 'Name,
        'tt1 ::> 'TType,
        'tt2 ::> 'TType,
        'tt3 ::> 'TType,
        't1 ::> 'Table,
        't2 ::> 'Table,
        't3 ::> 'Table),
      test8goal
    )

    val pg = makeSingleNodeProofGraph("test-8 goal", testspec, test8)

    val obl = pg.findObligation("test-8 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-9 goal") {
    val test9goal: Goals = goal(
      (~'TTC === 'bindContext ('tn1, 'tt, 'bindContext ('tn2, 'tt, 'bindContext ('tn3, 'tt, 'bindContext ('tn4, 'tt4, 'bindContext ('tn5, 'tt, 'emptyContext)))))
        ).===>("test-9")(
        'lookupContext ('tn4, ~'TTC) === 'someTType ('tt4)
      ))

    val test9: Local = local(
      differentconsts('tn1 ::> 'Name,
        'tn2 ::> 'Name,
        'tn3 ::> 'Name,
        'tn4 ::> 'Name,
        'tn5 ::> 'Name,
        'tt ::> 'TType,
        'tt4 ::> 'TType),
      test9goal
    )

    val pg = makeSingleNodeProofGraph("test-9 goal", testspec, test9)

    val obl = pg.findObligation("test-9 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }

  test("Verify ProofGraph for test-10 goal") {
    val test10goal: Goals = goal(
      ((~'row1 === 'rcons ('v1, 'rcons ('v2, 'rempty))) &
        (~'row2 === 'rcons ('v3, 'rcons ('v4, 'rempty))) &
        (~'table === 'tcons (~'row1, 'tcons (~'row2, 'tempty))) &
        (~'result === 'dropFirstColRaw (~'table)) &
        (~'resultRow1 === 'rcons ('v2, 'rempty)) &
        (~'resultRow2 === 'rcons ('v4, 'rempty))
        ).===>("test-10")(
        ~'result === 'tcons (~'resultRow1, 'tcons (~'resultRow2, 'tempty))
      ))

    val test10: Local = local(
      differentconsts('v1 ::> 'Val,
        'v2 ::> 'Val,
        'v3 ::> 'Val,
        'v4 ::> 'Val),
      test10goal
    )

    val pg = makeSingleNodeProofGraph("test-10 goal", testspec, test10)

    val obl = pg.findObligation("test-10 goal")
    val proofstep = pg.appliedStep(obl.get).get
    val nonResult = pg.verifiedBy(proofstep)
    assert(nonResult.isEmpty)

    val verifier = MockAlwaysVerifier[VeritasConstruct, VeritasConstruct]()
    val result = verifier.verify(obl.get.goal, obl.get.spec, Nil, None, pg.stepResultProducer)
    pg.setVerifiedBy(proofstep, result)

    val retrievedResult = pg.verifiedBy(proofstep)
    assert(retrievedResult.isDefined)
    assert(retrievedResult.get.status.isVerified == result.status.isVerified)
    assert(retrievedResult.get.status == result.status)
    assert(retrievedResult.get.errorMsg == result.errorMsg)
    assert(retrievedResult.get.evidence == result.evidence)
  }
}
