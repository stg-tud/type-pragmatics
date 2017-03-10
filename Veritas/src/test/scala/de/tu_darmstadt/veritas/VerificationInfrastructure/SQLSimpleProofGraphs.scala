package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite
import quiver.LNode

/**
  * Created by sylvia on 22/02/2017.
  */
class SQLSimpleProofGraphs extends FunSuite {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.testexamples.SQLDefs.{Tables, TableAux, TStore,
  TContext, Syntax, Semantics, TypeSystem, TypeSystemInv, SoundnessAuxDefs}

  // We instantiate S = Seq[VeritasConstruct] and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).

  val testspec = Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ SoundnessAuxDefs.defs

  def makeSingleNodeProofGraph(nodename: String, tspec: Seq[VeritasConstruct], goal: VeritasConstruct):
  ProofGraphQuiver[Seq[VeritasConstruct], VeritasConstruct] = {
    val proofnode: ProofNode[Seq[VeritasConstruct], VeritasConstruct] =
      LNode(nodename, ProofStep[Seq[VeritasConstruct], VeritasConstruct](tspec, goal))
    ProofGraphQuiver(Seq(proofnode))
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
    assert(!pg.computeFullyVerified("test-1 goal"))

    //TODO: add assertions for verification of test-1 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-2 goal"))

    //TODO: add assertions for verification of test-2 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-3 goal"))

    //TODO: add assertions for verification of test-3 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-4 goal"))

    //TODO: add assertions for verification of test-4 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-5 goal"))

    //TODO: add assertions for verification of test-5 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-6 goal"))

    //TODO: add assertions for verification of test-6 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-7 goal"))

    //TODO: add assertions for verification of test-7 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-8 goal"))

    //TODO: add assertions for verification of test-8 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-9 goal"))

    //TODO: add assertions for verification of test-9 (using concrete verifiers and transformers)

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
    assert(!pg.computeFullyVerified("test-10 goal"))

    //TODO: add assertions for verification of test-10 (using concrete verifiers and transformers)

  }

}
