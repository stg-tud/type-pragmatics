package de.tu_darmstadt.veritas.sudoku

/*
represents a single Sudoku cell;
convention: 0 represents a cell for which a value is not yet known
if a value is known, the candidate set has to be empty
 */
case class SudokuCell(value: Int, candidates: Set[Int]) {
  require(value == 0 || candidates.isEmpty, "A cell with a known value cannot have candidates.")

  def valid(cellrange: Range) = (cellrange contains value) && (candidates forall (cellrange contains _))

}

/**
  * Representation of a Sudoku field, with domain-specific queries
  */
class SudokuField(val field: Field) {
  require(dimensionsCorrect(), "The given field does not have the correct dimensions.")
  require(validCells(), s"Not all cells had the correct range (${cellrange})")

  /*
  alternative constructor for parsing a String representation of a Sudoku
   */
  def this(sfield: String) = this(SudokuField.parseStringRepr(sfield))

  val cellrange : Range = 1 to 9

  private def correctLength[T](f: Array[T]): Boolean = f.length == cellrange.max

  private def dimensionsCorrect(): Boolean = {
    correctLength(field) &&
      field.forall(correctLength(_))
  }

  private def validCells(): Boolean = cells forall (_.valid(cellrange))

  def cells: Seq[SudokuCell] = field.fold(Array())(_ ++ _)

  def rows: Seq[Row] = field

  def row(i: Int): Row =
    if (cellrange contains i) field(i-1) else sys.error(s"Attempted to access a row that is out of range ($i).")

  def column(i: Int): Column = ???

  def columns: Seq[Column] = ???
}

object SudokuField {
  def parseStringRepr(sfield: String): Field = ???
}
