package de.tu_darmstadt.veritas.sudoku

import z3.scala._
import z3.scala.dsl.{Distinct, IntVar}

import scala.reflect.ClassTag
import scala.util.Random

/**
  * Z3 Sudoku solver
  */
class SudokuZ3(val swidth: Int = 9,
               val sheight: Int = 9,
               val blank: String = ".",
               val symrange: String = ".123456789",
               val boxmap: Map[Int, (Range, Range)] = Map(
                 0 -> (0 to 2, 0 to 2),
                 1 -> (0 to 2, 3 to 5),
                 2 -> (0 to 2, 6 to 8),
                 3 -> (3 to 5, 0 to 2),
                 4 -> (3 to 5, 3 to 5),
                 5 -> (3 to 5, 6 to 8),
                 6 -> (6 to 8, 0 to 2),
                 7 -> (6 to 8, 3 to 5),
                 8 -> (6 to 8, 6 to 8))
              ) {
  val symmap: Map[Int, Char] = (for (c <- symrange) yield symrange.indexOf(c) -> c).toMap


  //ignore error handling
  def row[A](arr: Array[Array[A]], i: Int): Array[A] = arr(i)

  def col[A: ClassTag](arr: Array[Array[A]], i: Int): Array[A] = (for (r <- arr) yield r(i))

  def box[A: ClassTag](arr: Array[Array[A]], i: Int): Array[A] = {
    val (rowrange, colrange) = boxmap(i)
    val rows = arr.slice(rowrange.min, rowrange.max + 1)
    (for (r <- rows) yield r.slice(colrange.min, colrange.max + 1)).fold(Array())(_ ++ _)
  }

  //function for making the difference constraints (rows, columns, boxes)
  val diffConstr = (s: Seq[IntVar]) => Distinct(s: _*)

  def parseSudoku(sudoku: String): Array[Array[Int]] = {
    val parsedsudoku: Array[Array[Char]] = (sudoku.sliding(swidth, swidth).toArray).map(_.toCharArray)

    //translate symbols to integers
    val translatedsudoku: Array[Array[Int]] =
      for (r <- parsedsudoku) yield
        for (c <- r) yield
          (symmap.find { case (i, ch) => ch == c }).get._1

    translatedsudoku
  }

  def makeSolutionVariables(): Array[Array[IntVar]] =
    (for (i <- 0 until swidth) yield
      (for (j <- 0 until sheight) yield IntVar()).toArray).toArray


  def addConstraints(solver: Z3Solver, cells: Array[Array[IntVar]], translatedsudoku: Array[Array[Int]]): Unit = {
    //bounds for cell values
    val boundsConstr = for (r <- cells; c <- r) yield (c >= 1 && c < symmap.size)

    //constraints for given values
    val givenConstr = for (i <- 0 until sheight; j <- 0 until swidth
                           if (translatedsudoku(i)(j) != 0)) yield cells(i)(j) === translatedsudoku(i)(j)


    boundsConstr map (solver.assertCnstr(_))
    givenConstr map (solver.assertCnstr(_))

    //uniqueness constraints for rows, columns, boxes
    val rowConstr = for (i <- 0 until sheight) yield diffConstr(row(cells, i))
    val colConstr = for (i <- 0 until swidth) yield diffConstr(col(cells, i))
    val boxConstr = for (i <- 0 until boxmap.size) yield diffConstr(box(cells, i))


    rowConstr map (solver.assertCnstr(_))
    colConstr map (solver.assertCnstr(_))
    boxConstr map (solver.assertCnstr(_))

  }

  def checkUnique(sudoku: String, solution: String): Boolean = {
    val translatedsudoku = parseSudoku(sudoku)
    val translatedsolution = parseSudoku(solution)

    val z3 = new Z3Context("MODEL" -> true)

    val cells = makeSolutionVariables()

    val solver = z3.mkSolver

    addConstraints(solver, cells, translatedsudoku)

    val singlesolConstr = for (i <- 0 until sheight; j <- 0 until swidth) yield (cells(i)(j) === translatedsolution(i)(j)).ast(z3)
    val solConstr = z3.mkNot(z3.mkAnd(singlesolConstr: _*))

    solver.assertCnstr(solConstr)

    solver.check() match {
      case None => println("Uniqueness check did not terminate!"); false
      case Some(false) => true
      case Some(true) => false
    }


  }


  //takes 2D Sudokus via single line strings (left to right, top to bottom)
  def solveSingle(sudoku: String): String = {

    //translate symbols to integers
    val translatedsudoku: Array[Array[Int]] = parseSudoku(sudoku)

    val z3 = new Z3Context("MODEL" -> true)
    //val z3 = new Z3Context("MODEL" -> true, "timeout" -> 1000) //timeout is in milliseconds

    /**
      * The following parameters can be set:
      *     - proof  (Boolean)           Enable proof generation
      *     - debug_ref_count (Boolean)  Enable debug support for Z3_ast reference counting
      *     - trace  (Boolean)           Tracing support for VCC
      *     - trace_file_name (String)   Trace out file for VCC traces
      *     - timeout (unsigned)         default timeout (in milliseconds) used for solvers
      *     - well_sorted_check          type checker
      *     - auto_config                use heuristics to automatically select solver and configure it
      *     - model                      model generation for solvers, this parameter can be overwritten when creating a solver
      *     - model_validate             validate models produced by solvers
      *     - unsat_core                 unsat-core generation for solvers, this parameter can be overwritten when creating a solver
      **/

    //make integer variables for every Sudoku cell
    val cells = makeSolutionVariables()

    val solver = z3.mkSolver

    addConstraints(solver, cells, translatedsudoku)

    val result: String = solver.check() match {
      case None => "Z3 failed. The reason is: " + solver.getReasonUnknown()
      case Some(false) => "Unsatisfiable."
      case Some(true) => {
        val m = solver.getModel()
        makeSudokuString[IntVar](cells, (v : IntVar) => m.evalAs[Int](v.ast(z3)).get)
      }
    }

    z3.delete()
    result
  }

  def chooseRandomNewPosition(r: Random, sudoku: Array[Array[Int]]): (Int, Int) = {
    var randx = 0
    var randy = 0
    do {
      randx = r.nextInt(sheight)
      randy = r.nextInt(swidth)
    } while (sudoku(randx)(randy) == 0)
    (randx, randy)
  }

  def makeSudokuString[A](sudokuArr: Array[Array[A]], f: A => Int): String =
    (for (i <- 0 until sheight; j <- 0 until swidth) yield {
      val index: Int = f(sudokuArr(i)(j))
      symmap(index)
    }).mkString("")

  // generate a Sudoku with a unique solution from a full solution
  def generateSudoku(solution: String, maxiterations: Int = 0): String = {
    val translatedres = parseSudoku(solution)
    var resultsudoku = ""
    var it: Int = 0
    do {
      val (rx, ry) = chooseRandomNewPosition(Random, translatedres)
      resultsudoku = makeSudokuString(translatedres, (x : Int) => x)
      translatedres(rx)(ry) = 0
      it += 1
    } while (checkUnique(makeSudokuString(translatedres, (x : Int) => x), solution) && it != maxiterations+1)
    resultsudoku
  }


}
