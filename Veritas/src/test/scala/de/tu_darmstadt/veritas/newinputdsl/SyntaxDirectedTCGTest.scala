package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.newinputdsl.typechecker.{ReflectionHelper, SyntaxDirectedTypeCheckerGenerator}
import de.tu_darmstadt.veritas.newinputdsl.QLSpec._
import org.scalatest.FunSuite

class SyntaxDirectedTCGTest extends FunSuite {
  private val qlSpecFile =
    new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl/QLSpec.scala")
  private val qlSpecString = scala.io.Source.fromFile(qlSpecFile).mkString("")
  private val generator = new SyntaxDirectedTypeCheckerGenerator[QLSpec.type, QLSpec.Context, QLSpec.Expression, QLSpec.Type] {}
  private val typechecker = generator.generate(qlSpecString)



  trait WrappedStrinRep {
    override def toString: String = "\"" + super.toString + "\""
  }
  private val qid1 = new QID with WrappedStrinRep {}
  private val qid2 = new QID with WrappedStrinRep {}
  private val gid1 = new GID with WrappedStrinRep {}
  private val label1 = new Label with WrappedStrinRep {}
  ReflectionHelper.registerTerm(qid1)
  ReflectionHelper.registerTerm(qid2)
  ReflectionHelper.registerTerm(gid1)
  ReflectionHelper.registerTerm(label1)
  test ("simple qseq") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qseq(qempty(), qempty()),
      MC(atmempty(), atmempty())))
  }

  test ("simple ask") {
    assert(typechecker.typable(
      MC(atmempty(), atmbind(qid1, YesNo(), atmempty())),
      qsingle(ask(qid1)),
      MC(atmbind(qid1, YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple ask") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(ask(qid1)),
      MC(atmbind(qid1, YesNo(), atmempty()), atmempty())))
  }

  test ("simple question") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(question(qid1, label1, YesNo())),
      MC(atmbind(qid1, YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple question") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(question(qid1, label1, YesNo())),
      MC(atmbind(qid1, Number(), atmempty()), atmempty())))
  }

  test ("simple value") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(value(qid1, YesNo(), binop(constant(B(no())), eqop(), constant(B(yes()))))),
      MC(atmbind(qid1, YesNo(), atmempty()), atmempty())))
  }

  test ("failed simple value") {
    assert(!typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(value(qid1, YesNo(), binop(constant(B(no())), eqop(), constant(B(yes()))))),
      MC(atmbind(qid1, YesNo(), atmempty()), atmbind(qid2, Text(), atmempty()))))
  }

  test ("simple defquestion") {
    assert(typechecker.typable(
      MC(atmempty(), atmempty()),
      qsingle(defquestion(qid1, label1, YesNo())),
      MC(atmempty(), atmbind(qid1, YesNo(), atmempty()))))
  }
}
