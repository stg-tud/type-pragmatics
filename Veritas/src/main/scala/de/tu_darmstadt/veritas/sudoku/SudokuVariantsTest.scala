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
    override def addConstraints(solver: Z3Solver, cells: Array[Array[IntVar]], translatedsudoku: Array[Array[Int]]): Unit = {
      super.addConstraints(solver, cells, translatedsudoku)

      def diagonal(i: Int): Array[IntVar] =
        (for (j <- 0 to 8) yield
          if (i == 1)
            cells(j)(j)
          else
            cells(8 - j)(j)).toArray

      val diag1Constr = diffConstr(diagonal(1))
      val diag2Constr = diffConstr(diagonal(2))

      solver.assertCnstr(diag1Constr)
      solver.assertCnstr(diag2Constr)

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

  val giant25sudoku = new SudokuZ3(25, 25, ".", ".ABCDEFGHIJKLMNOPQRSTUVWXY",
    Map(
      0 -> (0 to 4, 0 to 4),
      1 -> (0 to 4, 5 to 9),
      2 -> (0 to 4, 10 to 14),
      3 -> (0 to 4, 15 to 19),
      4 -> (0 to 4, 20 to 24),

      5 -> (5 to 9, 0 to 4),
      6 -> (5 to 9, 5 to 9),
      7 -> (5 to 9, 10 to 14),
      8 -> (5 to 9, 15 to 19),
      9 -> (5 to 9, 20 to 24),

      10 -> (10 to 14, 0 to 4),
      11 -> (10 to 14, 5 to 9),
      12 -> (10 to 14, 10 to 14),
      13 -> (10 to 14, 15 to 19),
      14 -> (10 to 14, 20 to 24),

      15 -> (15 to 19, 0 to 4),
      16 -> (15 to 19, 5 to 9),
      17 -> (15 to 19, 10 to 14),
      18 -> (15 to 19, 15 to 19),
      19 -> (15 to 19, 20 to 24),

      20 -> (20 to 24, 0 to 4),
      21 -> (20 to 24, 5 to 9),
      22 -> (20 to 24, 10 to 14),
      23 -> (20 to 24, 15 to 19),
      24 -> (20 to 24, 20 to 24)
    ))


  def sudokusFromFile(path: String): Map[String, String] = {
    val flines = Source.fromFile(path).getLines().toList
    (for (l <- flines.sliding(2, 3)) yield l.head -> l.last).toMap
  }

  def prettySudokuFromFile(path: String): String = {
    val flines = Source.fromFile(path).getLines().toList
    flines.mkString("")
  }

  //  println("Normal Sudoku:")
  //  for ((field, solution) <- sudokusFromFile("sudokupuzzles/difficult9times9sudokus")) {
  //    val sol: String = standardSudokus.solveSingle(field)
  //    println(sol)
  //    assert(sol == solution)
  //    assert(standardSudokus.checkUnique(field, sol))
  //  }
  //
  //  println("Windoku:")
  //  for ((field, solution) <- sudokusFromFile("sudokupuzzles/windokus")) {
  //    val sol: String = windoku.solveSingle(field)
  //    println(sol)
  //    assert(sol == solution)
  //    assert(windoku.checkUnique(field, sol))
  //  }
  //
  //  println("SudokuX: ")
  //  for ((field, solution) <- sudokusFromFile("sudokupuzzles/sudokuX")) {
  //    val sol: String = sudokuX.solveSingle(field)
  //    println(sol)
  //    assert(sol == solution)
  //    assert(sudokuX.checkUnique(field, sol))
  //  }
  //
  //  println("Giant Sudoku:")
  //  for ((field, solution) <- sudokusFromFile("sudokupuzzles/giantsudoku")) {
  //    val sol: String = giantsudoku.solveSingle(field)
  //    println(sol)
  //    assert(!giantsudoku.checkUnique(field, sol))
  //  }

//  println("Normal Sudoku generation:")
//  for ((field, solution) <- sudokusFromFile("sudokupuzzles/difficult9times9sudokus")) {
//    val gsudoku = standardSudokus.generateSudoku(solution)
//    println(gsudoku)
//    assert(standardSudokus.checkUnique(gsudoku, solution))
//  }
//
//  println("Giant Sudoku generation:")
//  for ((field, solution) <- sudokusFromFile("sudokupuzzles/giantsudoku")) {
//    val gsudoku: String = giantsudoku.generateSudoku(solution)
//    println(gsudoku)
//    assert(giantsudoku.checkUnique(gsudoku, solution))
//  }
//
  println("Giant 25x25 Sudoku solution:")
  val gsudoku: String = prettySudokuFromFile("sudokupuzzles/giant25sudoku")
  println(gsudoku)
  val gsol: String = giant25sudoku.solveSingle(gsudoku)
  println(gsol)

}
