package de.tu_darmstadt.veritas

/**
  * type aliases for Sudoku
  */
package object sudoku {
  type Row = Array[SudokuCell]
  type Column = Array[SudokuCell]
  type Box = Array[Array[SudokuCell]]
  type Field = Array[Row]
  type SudokuUnit = Seq[SudokuCell]
  type Position = (Int, Int) //(rowindex, colindex); both indices always count from 1 on!
}
