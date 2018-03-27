package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.newinputdsl.typechecker.SyntaxDirectedTypeCheckerGenerator
import de.tu_darmstadt.veritas.newinputdsl.QLSpec._
import org.scalatest.FunSuite

class SyntaxDirectedTCGTest extends FunSuite {
  private val qlSpecFile =
    new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl/QLSpec.scala")
  private val qlSpecString = scala.io.Source.fromFile(qlSpecFile).mkString("")
  private val generator = new SyntaxDirectedTypeCheckerGenerator[QLSpec.type, QLSpec.Context, QLSpec.Expression, QLSpec.Type] {}
  private val typechecker = generator.generate(qlSpecString)

  test ("simple qseq") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qseq(qempty(), qempty()),
      MC(atmempty(), atmempty())))
  }

  test ("simple ask") {
    assert(typechecker.typable(
      MC(atmempty(), atmbind(someID(zero()), YesNo(), atmempty())),
      qsingle(ask(someID(zero()))),
      MC(atmbind(someID(zero()), YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple ask") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(ask(someID(zero()))),
      MC(atmbind(someID(zero()), YesNo(), atmempty()), atmempty())))
  }

  test ("simple question") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(question(someID(zero()), someLabel(zero()), YesNo())),
      MC(atmbind(someID(zero()), YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple question") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(question(someID(zero()), someLabel(zero()), YesNo())),
      MC(atmbind(someID(zero()), Number(), atmempty()), atmempty())))
  }

  test ("simple value") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(value(someID(zero()), YesNo(), binop(constant(B(no())), eqop(), constant(B(yes()))))),
      MC(atmbind(someID(zero()), YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple value") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(value(someID(zero()), YesNo(), binop(constant(B(no())), eqop(), constant(B(yes()))))),
      MC(atmbind(someID(zero()), YesNo(), atmempty()), atmbind(someID(succ(zero())), Text(), atmempty()))))
  }

  test ("simple defquestion") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(defquestion(someID(zero()), someLabel(zero()), YesNo())),
      MC(atmempty(), atmbind(someID(zero()), YesNo(), atmempty()))))
  }
}
