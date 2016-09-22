package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._


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

  val testtree = 'zero | ('succ('nat - 'nat))

  val testDT = data ('nat) of 'zero | 'succ('nat)

  val rawDT = data ('nat) of DataTypeConstructor("zero", Seq()) | DataTypeConstructor("succ", Seq(SortRef("nat")))


  val funeqtest: FunctionEq = 'plus('x - 'zero) := (FunctionExpVar("x"))
  val funeqtest2: FunctionEq = 'plus('x - 'succ('y)) := FunctionExpApp("succ", Seq(FunctionExpApp("plus", Seq(FunctionExpVar("x"), FunctionExpVar("y")))))
  val testfun: Functions = function ('plus >> 'nat - 'nat -> 'nat)
      ('plus('x - 'zero) := FunctionExpVar("x")) |
      ('plus('x - 'succ('y)) := FunctionExpApp("succ", Seq(FunctionExpApp("plus", Seq(FunctionExpVar("x"), FunctionExpVar("y"))))))

}