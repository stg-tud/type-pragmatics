package de.tu_darmstadt.veritas.sudoku

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{SolveWithZ3, Verifier, VerifierHints}
import z3.scala.Z3Model
import z3.scala.dsl.{Distinct, IntVar}

import scala.reflect.ClassTag

/**
  * Trying to apply Z3 to verify a Sudoku directly
  * Must not take longer than 3 seconds (design decision)
  */
class SudokuLeafVerifier(config: Map[String, Any] = Map("MODEL" -> true, "timeout" -> 3000))
  extends SolveWithZ3[EmptySpec, SudokuField](config) with Verifier[EmptySpec, SudokuField] {

  override type Var = Array[IntVar] //??? no particular verification format needed

  //function for making the difference constraints (rows, columns, boxes)
  protected val diffConstr = (s: Seq[IntVar]) => Distinct(s: _*)

  //local accessor functions
  //ignore error handling
  protected def row[A](arr: Array[Array[A]], i: Int): Array[A] = arr(i)

  protected def col[A: ClassTag](arr: Array[Array[A]], i: Int): Array[A] = (for (r <- arr) yield r(i))

  protected def box[A: ClassTag](arr: Array[Array[A]], i: Int, boxmap: Map[Int, (Range, Range)]): Array[A] = {
    val (rowrange, colrange) = boxmap(i)
    val rows = arr.slice(rowrange.min, rowrange.max + 1)
    (for (r <- rows) yield r.slice(colrange.min, colrange.max + 1)).fold(Array())(_ ++ _)
  }


  override def makeSolutionVariables(goal: SudokuField): Array[Array[IntVar]] =
    (for (i <- 0 until goal.rownum) yield
      (for (j <- 0 until goal.colnum) yield IntVar()).toArray).toArray

  override def makeAndAddConstraints(spec: EmptySpec, goal: SudokuField,
                                     vars: Array[Array[IntVar]]): Unit = {

    val boxmap = goal.boxindices

    //bounds for cell values
    val boundsConstr = for (r <- vars; c <- r) yield (c >= 1 && c <= goal.rownum)
    boundsConstr map (solver.assertCnstr(_))

    //constraints for given values in Sudoku
    val givenConstr = for (i <- 0 until goal.rownum; j <- 0 until goal.colnum
                           if (goal.field(i)(j).value != 0)) yield vars(i)(j) === goal.field(i)(j).value
    givenConstr map (solver.assertCnstr(_))

    //uniqueness constraints for rows, columns, boxes
    val rowConstr = for (i <- 0 until goal.rownum) yield diffConstr(row(vars, i))
    val colConstr = for (i <- 0 until goal.colnum) yield diffConstr(col(vars, i))
    val boxConstr = for (i <- 0 until boxmap.size) yield diffConstr(box(vars, i, boxmap))


    rowConstr map (solver.assertCnstr(_))
    colConstr map (solver.assertCnstr(_))
    boxConstr map (solver.assertCnstr(_))


  }

  override def parseResult(m: Z3Model, vars: Array[Array[IntVar]]): String =
    (for (i <- 0 until vars.length; j <- 0 until vars(0).length) yield {
      m.evalAs[Int](vars(i)(j).ast(context)).get
    }).mkString("")

  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "Sudoku leaf verifier"

  override def verify[Result <: GenStepResult[EmptySpec, SudokuField]]
  (goal: SudokuField,
   spec: EmptySpec,
   parentedges: Iterable[EdgeLabel],
   assumptions: Iterable[SudokuField],
   hints: Option[VerifierHints],
   produce: StepResultProducer[EmptySpec, SudokuField, Result]): Result = ???
}
