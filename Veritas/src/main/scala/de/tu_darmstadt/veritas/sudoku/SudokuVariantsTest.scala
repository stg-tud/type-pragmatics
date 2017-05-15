package de.tu_darmstadt.veritas.sudoku

import z3.scala.Z3Solver
import z3.scala.dsl.IntVar

import scala.io.Source

/**
  * Created by sylvia on 15.05.17.
  */
object SudokuVariantsTest extends App {

  val standardSudokus = new SudokuZ3()
  val sudoku6times6 = new SudokuZ3(6, 6, ".", ".123456",
    Map(
      0 -> (0 to 1, 0 to 2),
      1 -> (0 to 1, 3 to 5),
      2 -> (2 to 3, 0 to 2),
      3 -> (2 to 3, 3 to 5),
      4 -> (4 to 5, 0 to 2),
      5 -> (4 to 5, 3 to 5)
    ))

  val windoku = new SudokuZ3(9, 9, ".", ".123456789",
    Map(
      0 -> (0 to 2, 0 to 2),
      1 -> (0 to 2, 3 to 5),
      2 -> (0 to 2, 6 to 8),
      3 -> (3 to 5, 0 to 2),
      4 -> (3 to 5, 3 to 5),
      5 -> (3 to 5, 6 to 8),
      6 -> (6 to 8, 0 to 2),
      7 -> (6 to 8, 3 to 5),
      8 -> (6 to 8, 6 to 8),
      9 -> (1 to 3, 1 to 3),
      10 -> (1 to 3, 5 to 7),
      11 -> (5 to 7, 1 to 3),
      12 -> (5 to 7, 5 to 7)
    ))

  val sudokuX = new SudokuZ3() {
    override def addConstraints(solver: Z3Solver, cells: Array[Array[IntVar]]): Unit = {
      def diagonal(i: Int): Array[IntVar] =
        (for (j <- 0 to 8) yield
          if (i == 1)
            cells(j)(j)
          else
            cells(8-j)(j)).toArray

      val diag1Constr = diffConstr(diagonal(1))
      val diag2Constr = diffConstr(diagonal(2))

      solver.assertCnstr(diag1Constr)
      solver.assertCnstr(diag2Constr)
      super.addConstraints(solver, cells)
    }

  }

  val giantsudoku = new SudokuZ3(16, 16, ".", ".123456789ABCDEFG",
  Map(
    0 -> (0 to 3, 0 to 3),
    1 -> (0 to 3, 4 to 7),
    2 -> (0 to 3, 8 to 11),
    3 -> (0 to 3, 12 to 15),
    4 -> (4 to 7, 0 to 3),
    5 -> (4 to 7, 4 to 7),
    6 -> (4 to 7, 8 to 11),
    7 -> (4 to 7, 12 to 15),
    8 -> (8 to 11, 0 to 3),
    9 -> (8 to 11, 4 to 7),
    10 -> (8 to 11, 8 to 11),
    11 -> (8 to 11, 12 to 15),
    12 -> (12 to 15, 0 to 3),
    13 -> (12 to 15, 4 to 7),
    14 -> (12 to 15, 8 to 11),
    15 -> (12 to 15, 12 to 15)
  ))

  def sudokusFromFile(path: String): Map[String, String] = {
    val flines = Source.fromFile(path).getLines().toList
    (for (l <- flines.sliding(2, 3)) yield l.head -> l.last).toMap
  }

  println("Normal Sudoku:")
  for ((field, solution) <- sudokusFromFile("sudokupuzzles/difficult9times9sudokus")) {
    val sol: String = standardSudokus.solveSingle(field)
    println(sol)
    assert(sol == solution)
  }

  println("Windoku:")
  for ((field, solution) <- sudokusFromFile("sudokupuzzles/windokus")) {
    val sol: String = windoku.solveSingle(field)
    println(sol)
    assert(sol == solution)
  }

  println("SudokuX: ")
  for ((field, solution) <- sudokusFromFile("sudokupuzzles/sudokuX")) {
    val sol: String = sudokuX.solveSingle(field)
    println(sol)
    assert(sol == solution)
  }

  println("Giant Sudoku:")
  for ((field, solution) <- sudokusFromFile("sudokupuzzles/giantsudoku")) {
    val sol: String = giantsudoku.solveSingle(field)
    println(sol)
  }


}
