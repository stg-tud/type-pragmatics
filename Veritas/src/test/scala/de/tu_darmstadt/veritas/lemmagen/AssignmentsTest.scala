package de.tu_darmstadt.veritas.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint, FreshChoice, VariableChoice}
import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import org.scalatest.FunSuite

/** Some small tests for Assignments */
class AssignmentsTest extends FunSuite {
  val TInt = SortRef("Int")
  val TString = SortRef("String")

  val zero = MetaVar("zero")
  zero.typ = Some(TypeInference.Sort("Int"))
  val one = MetaVar("one")
  one.typ = Some(TypeInference.Sort("Int"))

  val hello = MetaVar("hello")
  hello.typ = Some(TypeInference.Sort("String"))
  val world = MetaVar("world")
  world.typ = Some(TypeInference.Sort("String"))

  test("Constraints are translated to abstract and concrete assignments") {
    val types = Seq(TInt, TString, TString)

    // if all variables are fresh, there is only one choice
    val assignment1 = Assignments.generateAbstractAssignments(Constraint.fresh(types), Set[MetaVar]())
    assertResult(Seq(Seq(FreshChoice(TInt), FreshChoice(TString), FreshChoice(TString)))) {
      assignment1
    }

    // concrete choice
    assertResult(Seq(MetaVar("i"), MetaVar("s"), MetaVar("s1"))) {
      Assignments.generateConcreteAssignment(assignment1.head, Set())
    }
    // correct types
    assertResult(types) {
      Assignments.generateConcreteAssignment(assignment1.head, Set()).map(_.sortType)
    }

    // concrete choice with bound conflicting variables
    assertResult(Seq(MetaVar("i1"), MetaVar("s"), MetaVar("s1"))) {
      Assignments.generateConcreteAssignment(assignment1.head, Set(MetaVar("i")))
    }
    assertResult(Seq(MetaVar("i"), MetaVar("s"), MetaVar("s2"))) {
      Assignments.generateConcreteAssignment(assignment1.head, Set(MetaVar("s1"), MetaVar("s4")))
    }

    assertResult(Seq(Seq(FreshChoice(TInt), FreshChoice(TString), FreshChoice(TString)))) {
      Assignments.generateAbstractAssignments(Constraint.preferBound(types), Set())
    }

    // preferBound with ``zero`` bound, also only one choice
    var bound = Set(zero)
    val assignment2 = Assignments.generateAbstractAssignments(Constraint.preferBound(types), bound)
    assertResult(Seq(Seq(VariableChoice(zero), FreshChoice(TString), FreshChoice(TString)))) {
      assignment2
    }
    assertResult(Seq("zero", "s", "s1").map(MetaVar(_))) {
      Assignments.generateConcreteAssignment(assignment2.head, Set(zero))
    }

    // boundOrFresh with ``zero`` and ``one`` bound
    bound = Set(zero, one)
    val assignment3 = Assignments.generateAbstractAssignments(Constraint.freshOrBound(types), bound)
    assertResult(Set(
      Seq(VariableChoice(zero), FreshChoice(TString), FreshChoice(TString)),
      Seq(VariableChoice(one), FreshChoice(TString), FreshChoice(TString)),
      Seq(FreshChoice(TInt), FreshChoice(TString), FreshChoice(TString))
    )) {
      assignment3.toSet
    }

    assertResult(Set(
      Seq("zero", "s", "s1"),
      Seq("one", "s", "s1"),
      Seq("i", "s", "s1")
    ).map(a => a.map(MetaVar(_)))) {
      assignment3.map(Assignments.generateConcreteAssignment(_, bound)).toSet
    }

    // preferBound, but ``zero`` can be fresh or bound
    bound = Set(zero, hello, world)
    val assignment4 = Assignments.generateAbstractAssignments(
      Constraint
        .preferBound(types)
        .updated(0, Constraint.freshOrBound(TInt)),
      bound)
    assertResult(Set(
      Seq(zero, hello, world).map(VariableChoice),
      Seq(zero, world, hello).map(VariableChoice),
      Seq(zero, hello, hello).map(VariableChoice),
      Seq(zero, world, world).map(VariableChoice),
      Seq(FreshChoice(TInt), VariableChoice(hello), VariableChoice(world)),
      Seq(FreshChoice(TInt), VariableChoice(world), VariableChoice(hello)),
      Seq(FreshChoice(TInt), VariableChoice(hello), VariableChoice(hello)),
      Seq(FreshChoice(TInt), VariableChoice(world), VariableChoice(world))
    )) {
      assignment4.toSet
    }
  }

  test("assignments and lemmas") {
    import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
    val problem = new Problem(file)
    implicit val enquirer = problem.enquirer
    val projectTableProgress = problem.dsk.properties.find(_.name == "projectTableProgress").get
    val onePremise = new Lemma("noPremises", Seq(projectTableProgress.premises.last), projectTableProgress.consequences)
    problem.enquirer.getAllVarTypes(onePremise)
    println(onePremise)
    for(functionName <- Seq("welltypedtable", "lookupStore")) {
      val function = problem.dsk.lookupByFunName(problem.dsk.staticFunctions ++ problem.dsk.dynamicFunctions, functionName).head
      println(functionName)
      println(Assignments.generateSimple(function.signature.in, onePremise))
      println(Assignments.generateSimple(Seq(function.successfulOutType), onePremise))
      println("----")
    }
  }
}
