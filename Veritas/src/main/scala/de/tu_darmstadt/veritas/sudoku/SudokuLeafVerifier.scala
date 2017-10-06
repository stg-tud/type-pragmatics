package de.tu_darmstadt.veritas.sudoku

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, Evidence, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import z3.scala.Z3Model
import z3.scala.dsl.{Distinct, IntVar}

import scala.reflect.ClassTag

class Z3Evidence(val model: String) extends Evidence

class Z3ResultDetails(val messstr: String, val mod: Option[String]) extends ResultDetails with Serializable {
  /**
    *
    * @return full logs of prover
    */
  override def fullLogs: String = messstr

  override def summaryDetails: String = mod.toString()

  override def proofEvidence: Option[Evidence] = mod map (m => new Z3Evidence(m))

  override def message: Option[String] =
    if (messstr.startsWith("Z3 failed.") || messstr.startsWith("Unsatisfiable."))
      Some(messstr) else None
}

/**
  * Trying to apply Z3 to verify a Sudoku directly
  * Must not take longer than 3 seconds (design decision)
  */
class SudokuLeafVerifier
  extends SolveWithZ3[EmptySpec, SudokuField] with Verifier[EmptySpec, SudokuField] {

  override type Var = Array[IntVar]

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

    //here we need the actual array indices
    val boxmap = goal.boxindices map {case (bi, (rowrange, colrange)) => bi - 1 -> ((rowrange.min - 1 until rowrange.max, colrange.min - 1 until colrange.max)) }

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

    //add remaining candidate constraints
    val candConstr = for (x <- 0 until goal.rownum; y <- 0 until goal.colnum;
      if goal.field(x)(y).value == 0) yield {
      val cconstr = for (cand <- goal.field(x)(y).candidates) yield (vars(x)(y) === cand)
      context.mkOr((cconstr map (_.ast(context))).toSeq : _*)
    }

    candConstr map (solver.assertCnstr(_))


  }

  override def parseResult(goal: SudokuField)(m: Z3Model, vars: Array[Array[IntVar]]): Array[String] =
    ((for (i <- 0 until vars.length; j <- 0 until vars(0).length) yield {
      m.evalAs[Int](vars(i)(j).ast(context)).get
    }) map (goal.config.symmap(_).toString)).toArray

  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "Sudoku leaf verifier"

  override def verify[Result <: GenStepResult[EmptySpec, SudokuField]]
  (goal: SudokuField,
   spec: EmptySpec,
   parentedges: Iterable[EdgeLabel],
   assumptions: Iterable[SudokuField],
   hints: Option[VerifierHints],
   produce: StepResultProducer[EmptySpec, SudokuField, Result],
   pathforlogs: Option[String] = None): Result = {
    val sresult = solveAndGetResult(spec, goal, makeSolutionVariables(goal))
    val pstatus: ProverStatus =
      if (sresult(0).startsWith("Z3 failed."))
        ProverFailure(new Z3ResultDetails(sresult(0), None))
      else if (sresult(0).startsWith("Unsatisfiable."))
        Inconclusive(new Z3ResultDetails(sresult(0), None))
      else if (sresult.length == goal.colnum * goal.rownum)
        Proved(new Z3ResultDetails(sresult.mkString(""), Some(solver.getModel().toString())))
      else
        ProverFailure(new Z3ResultDetails("Could not parse result.", None))

    val evidence = pstatus.proverResult.proofEvidence
    val errorMsg = pstatus.proverResult.message
    val verifierStat = Finished(pstatus, this)

    produce.newStepResult(verifierStat, evidence, errorMsg)
  }

}
