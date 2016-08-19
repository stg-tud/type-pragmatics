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

  val basemv = new UnderspecifiedMetaVar("Base")
  val typemv = new SpecifiedMetaVar("Type")

  val testsyntaxblock = new SyntaxBlock(Seq(basemv), Seq(typemv),
    Seq(new MetaVarDefinition(typemv.getRef, Seq(new SyntacticForm("basetype", Seq(basemv.getRef)),
      new SyntacticForm("arrow", Seq(typemv.getRef, typemv.getRef))))))

  import DataTypeDSL._
  import SortRefDSL._

  val testDT = data ('nat) of
     'zero | 'succ('nat)
  import FunctionDSL._

  val sortreftest: Seq[SortRef] = ('nat - 'nat)

  val testsig: FunctionSig = 'plus >> 'nat - 'nat -> 'nat

}