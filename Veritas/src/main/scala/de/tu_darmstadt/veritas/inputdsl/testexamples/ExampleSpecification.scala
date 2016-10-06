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

  val testtree1: SymNode = 'succ ('succ('nat, 'nat, 'succ ('nat)))

  val testopen1: DataType = open data 'test
  val testopen2 = open data 'test of 'succ('nat) | 'succ2 ('nat)

  val testsingle: DataType = data('single) of 'bla
  val testsimple: DataType = data('color) of 'red | 'blue | 'green
  val testclosednat: DataType = data('nat) of 'zero | 'succ ('nat)

  val testfunctionsigsimple = 'test.>>('nat) -> 'nat
  val testfunctionsig1: FunctionSig = 'plus.>>('nat, 'nat) -> 'nat

  val funeqtest1: FunctionEq = 'plus('x, 'zero) := FunctionExpVar("x")
  val funeqtest1a: FunctionEq = 'plus('x, 'zero) := 'x
  val funeqtest2: FunctionEq = 'plus('x, 'succ('y)) := FunctionExpApp("succ", Seq(FunctionExpApp("plus", Seq(FunctionExpVar("x"), FunctionExpVar("y")))))
  val funeqtest2a: FunctionEq = 'plus('x, 'succ('y)) := 'succ('plus('x, 'y))

  val mvtest: MetaVar = ~'bla

  val funexpeqtest: FunExpTree = 'x === 'p('y)
  val funexpneqtest: FunExpTree = 'x ~= 'y
  val funexpeqlongertest: FunExpTree = ('x === 'Y) <=> ('Y === 'x)
  val funexptreetest: FunExpTree = 'p('x) && 'q('x) || 't('x)

  val funeqtestbin: FunctionEq = 'f('x) := 'p('x) && 'q('x)

  val fullfuntest: Functions = function ('plus.>>('nat, 'nat) -> 'nat)
    ('plus('x, 'zero) := 'x) |
    ('plus('x, 'succ('y)) := 'succ('plus('x, 'y)))

}