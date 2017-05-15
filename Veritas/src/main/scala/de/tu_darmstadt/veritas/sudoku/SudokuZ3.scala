package de.tu_darmstadt.veritas.sudoku

import z3.scala._
import z3.scala.dsl.{Distinct, IntVar}

import scala.reflect.ClassTag

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

  def addConstraints(solver: Z3Solver, cells: Array[Array[IntVar]]): Unit = {
    //uniqueness constraints for rows, columns, boxes
    val rowConstr = for (i <- 0 until sheight) yield diffConstr(row(cells, i))
    val colConstr = for (i <- 0 until swidth) yield diffConstr(col(cells, i))
    val boxConstr = for (i <- 0 until boxmap.size) yield diffConstr(box(cells, i))


    rowConstr map (solver.assertCnstr(_))
    colConstr map (solver.assertCnstr(_))
    boxConstr map (solver.assertCnstr(_))

  }


  //takes 2D Sudokus via single line strings (left to right, top to bottom)
  def solveSingle(sudoku: String): String = {
    import dsl._

    val parsedsudoku: Array[Array[Char]] = (sudoku.sliding(swidth, swidth).toArray).map(_.toCharArray)

    //translate symbols to integers
    val translatedsudoku: Array[Array[Int]] =
      for (r <- parsedsudoku) yield
        for (c <- r) yield
          (symmap.find { case (i, ch) => ch == c }).get._1

    val z3 = new Z3Context("MODEL" -> true)
    //make integer variables for every Sudoku cell
    val cells: Array[Array[IntVar]] =
      (for (i <- 0 until swidth) yield
        (for (j <- 0 until sheight) yield IntVar()).toArray).toArray

    //bounds for cell values
    val boundsConstr = for (r <- cells; c <- r) yield (c >= 1 && c < symmap.size)

    //constraints for given values
    val givenConstr = for (i <- 0 until sheight; j <- 0 until swidth if (translatedsudoku(i)(j) != 0)) yield cells(i)(j) === translatedsudoku(i)(j)

    val solver = z3.mkSolver

    boundsConstr map (solver.assertCnstr(_))
    givenConstr map (solver.assertCnstr(_))

    addConstraints(solver, cells)

    val result: String = solver.check() match {
      case None => "Z3 failed. The reason is: " + solver.getReasonUnknown()
      case Some(false) => "Unsatisfiable."
      case Some(true) => {
        val m = solver.getModel()
        (for (i <- 0 until sheight; j <- 0 until swidth) yield {
          val index: Int = m.evalAs[Int](cells(i)(j).ast(z3)).get
          symmap(index)
        }).mkString("")
      }
    }

    z3.delete()
    result
  }

}
