package de.tu_darmstadt.veritas.inputdsl.testexamples

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}


object ExampleSpecification {

  val natmoduleRaw = Module(
    "Natural",
    Seq(),
    Seq(
      DataType(false, "nat", Seq(
        DataTypeConstructor("zero", Seq()),
        DataTypeConstructor("succ", Seq(SortRef("nat"))))),

      Functions(Seq(
        FunctionDef(FunctionSig("plus", Seq(SortRef("nat"), SortRef("nat")), SortRef("nat")),
          Seq(FunctionEq("plus", Seq(FunctionPatVar("x"), FunctionPatVar("zero")),
            FunctionExpVar("x")),
            FunctionEq("plus", Seq(FunctionPatVar("x"), FunctionPatApp("succ", Seq(FunctionPatVar("y")))),
              FunctionExpApp("succ", Seq(FunctionExpApp("plus", Seq(FunctionExpVar("x"), FunctionExpVar("y"))))))
          ))))
    )
  )


  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  val testtree1: SymNode = 'succ ('succ('nat, 'nat, 'succ ('nat)))

  val testopen1: DataType = open data 'test
  val testopen2: DataType = open data 'test of 'succ('nat) | 'succ2 ('nat)

  val testsingle: DataType = data('single) of 'bla
  val testsimple: DataType = data('color) of 'red | 'blue | 'green
  val testclosednat: DataType = data('nat) of 'zero | 'succ ('nat)

  val testfunctionsigsimple: FunctionSig = 'test.>>('nat) -> 'nat
  val testfunctionsig1: FunctionSig = 'plus.>>('nat, 'nat) -> 'nat

  val funeqtest1: FunctionEq = 'plus('x, 'zero) := FunctionExpVar("x")
  val funeqtest1a: FunctionEq = 'plus('x, 'zero) := 'x
  val funeqtest2: FunctionEq = 'plus('x, 'succ('y)) := FunctionExpApp("succ", Seq(FunctionExpApp("plus", Seq(FunctionExpVar("x"), FunctionExpVar("y")))))
  val funeqtest2a: FunctionEq = 'plus('x, 'succ('y)) := 'succ('plus('x, 'y))

  val mvtest: MVarNode = ~'bla

  val funexpeqtest: FunExpTree = 'x === 'p('y)
  val funexpneqtest: FunExpTree = 'x ~= 'y
  val funexpeqlongertest: FunExpTree = ('x === 'Y) <=> ('Y === 'x)
  val funexptreetest: FunExpTree = 'p('x) && 'q('x) || 't('x)

  val funeqtestbin: FunctionEq = 'f('x) := 'p('x) && 'q('x)

  val fullfuntest: Functions = function ('plus.>>('nat, 'nat) -> 'nat)
    ('plus('x, 'zero) := 'x) |
    ('plus('x, 'succ('y)) := 'succ('plus('x, 'y)))

  val iffuntest: Functions = function ('plus.>>('nat, 'nat) -> 'nat)
  'plus('x, 'y) := iff ('y === 'zero) th 'x els 'succ('plus('x, 'y))

  val iftest: IfNode = iff ('y === 'zero) th 'x els 'y

  val lettestpartial: LetNode = (let ('rec) := 'succ('plus('x, 'y))) in (iff ('y === 'zero) th 'x els 'rec)

  val lettest: Functions = function ('plus.>>('nat, 'nat) -> 'nat)
  'plus ('x, 'y) := (let('rec) := 'succ ('plus ('x, 'y))) in (iff('y === 'zero) th 'x els 'rec)

  val tjtest: TypingRuleJudgment = 'C |- 't :: 'T
  val tjstest: TypingJudgmentSimple = 't :: 'T

  val fjtest: FunctionExpJudgment = 'p('x) && 'q('x)

  val existstest: ExistsJudgment = exists (~'C) | ('C |- 't :: 'T) & ('p('x) && 'q('x))
  val foralltest: ForallJudgment = forall (~'C, ~'t, ~'T) | ('C |- 't :: 'T)

  val premempty: TypingRule = ===>("test")('C |- 't :: 'T)
  val typingruletest: TypingRule = ('C |- 't :: 'T).===>("test")('C |- 't :: 'T)
  val typingruletest1: TypingRule =
    ('C |- 't :: 'T
      ).===>("test")(
    ('C |- 't :: 'T) &
    ('C |- 't :: 'T))
  val typingruletest2: TypingRule =
    (('C |- 't :: 'T) &
      ('C |- 't :: 'T)
      ).===>("test")(
      'C |- 't :: 'T)

  val ortest: OrJudgment = OR (=>> ('C |- 't :: 'T))
  val ortest1: OrJudgment = OR (=>> (('C |- 't :: 'T) & ('C |- 't :: 'T)))
  val ortest2: OrJudgment = OR (
    =>> (exists (~'C) | ('C |- 't :: 'T) & ('p('x) && 'q('x))) |
    =>> ('C |- 't :: 'T))

  val axiomtest: Axioms = axiom (
    ===>("test")('C |- 't :: 'T))

  val lemmatest: Lemmas = lemma (20) (===>("test")('C |- 't :: 'T))

  val testconsts: Consts = consts ('a ::> 'T, 'b ::> 'G)
  val testdifferentconsts: Consts = differentconsts ('a ::> 'T)

  val metavartest: SymNode = 'f(~'a, ~'b)
  val metavartest2: EqNode = 'f(~'a, ~'b) === 'g(~'b)

  val tpjmetavartest: TypingJudgmentSimple = ~'t :: ~'T
  val tpjmetavartest2 : TypingRuleJudgment = ~'C |- ~'t :: ~'T
  
  val morecomplexmetavartest: TypingJudgment = 'bindContext(~'x, ~'Tx, 'bindContext(~'y, ~'Ty, ~'C)) |- ~'e :: ~'T

  val andnode: FunExpMetaTree = ('matchingAttrL ('tt, 'al) && 'welltypedRawtable ('tt, 't))
  val funeq: FunctionEq = 'matchingAttrL ('ttcons ('a1, 'f, 'tt), 'acons ('a2, 'al)) := ('a1 === 'a2) && 'matchingAttrL ('tt, 'al)
  val functioneq: FunctionEq = 'welltypedtable ('tt, 'table ('al, 't)) := ('matchingAttrL ('tt, 'al) && 'welltypedRawtable ('tt, 't))

}