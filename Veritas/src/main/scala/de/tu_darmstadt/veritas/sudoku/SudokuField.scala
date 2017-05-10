package de.tu_darmstadt.veritas.sudoku


/*
represents a single Sudoku cell;
convention: 0 represents a cell for which a value is not yet known
if a value is known, the candidate set has to be empty
 */
case class SudokuCell(value: Int, candidates: Set[Int]) {
  require(value == 0 || candidates.isEmpty, "A cell with a known value cannot have candidates.")

  def valid(cellrange: Range) = (value == 0 || (cellrange contains value)) && (candidates forall (cellrange contains _))

}

/**
  * Representation of a Sudoku field, with domain-specific queries
  * parametric in cellrange (standard: 1-9), but assumes quadratic Sudoku
  */
class SudokuField(val field: Field) {

  import SudokuField._

  require(dimensionsCorrect(), "The given field does not have the correct dimensions.")
  require(validCells(), s"Not all cells had the correct range (${cellrange})")
  require(rows forall (onerule(_)), "This is not a proper Sudoku: The one-rule is not satisfied for at least one row.")
  require(columns forall (onerule(_)), "This is not a proper Sudoku: The one-rule is not satisfied for at least one column.")
  require(boxes forall (onerule(_)), "This is not a proper Sudoku: The one-rule is not satisfied for at least one box.")

  /*
  alternative constructor for parsing a String representation of a Sudoku
   */
  def this(sfield: String) = this(SudokuField.parseStringRepr(sfield))


  private def correctLength[T](f: Array[T]): Boolean = f.length == cellrange.max

  private def dimensionsCorrect(): Boolean = {
    correctLength(field) &&
      field.forall(correctLength(_))
  }

  private def validCells(): Boolean = cells forall (_.valid(cellrange))

  def cells: Seq[SudokuCell] = field.fold(Array())(_ ++ _)

  def rows: Iterator[SudokuUnit] = field.map(_.toSeq).toIterator

  //counting rows from 1!
  def row(i: Int): Row =
    if (cellrange contains i) field(i - 1) else sys.error(s"Attempted to access a row that is out of range ($i).")

  //counting columns from 1!
  def column(i: Int): Column =
    if (cellrange contains i)
      for (r <- field) yield r(i - 1)
    else sys.error(s"Attempted to access a column that is out of range ($i).")

  def columns: Iterator[SudokuUnit] = (for (i <- cellrange) yield column(i)).map(_.toSeq).toIterator

  //counting boxes from 1, left to right, top to bottom:
  // 1 2 3
  // 4 5 6
  // 7 8 9
  def box(i: Int): Box = if (cellrange contains i) {

    val translatedindex = ((i - 1) / boxsize) * boxsize
    val cutrows = field.slice(translatedindex, translatedindex + boxsize)
    // cut columns
    for (r <- cutrows) yield r.slice(translatedindex, translatedindex + boxsize)
  } else sys.error(s"Attempted to access a box that is out of range ($i).")

  def realsboxes(): Seq[Box] = for (i <- cellrange) yield box(i)

  def boxelems(i: Int): SudokuUnit = box(i).fold(Array())(_ ++ _)
  def boxes(): Iterator[SudokuUnit] = (for (i <- cellrange) yield boxelems(i)).toIterator


  def onerule(unit: SudokuUnit): Boolean = {
    val cellvals = unit.map(_.value).filter(_ != 0)
    cellvals equals cellvals.distinct
  }

  //returns index of row that given cell positions share
  // returns zero if given cell positions do not share a row
  def shareRow(cells: Seq[Position]): Int = {
    val rows = (cells map (_._1)).distinct
    if (rows.length == 1)
      rows.head
    else
      0
  }

  //TODO: these methods should also hand back a position that allows you to change the cells (e.g. remove candidates)
  def rowPeers(cell: Position): Seq[SudokuCell] = row(shareRow(Seq(cell))).filter(_ != field(cell._1)(cell._2))

  //returns index of column that given cell positions share
  // returns zero if given cell positions do not share a column
  def shareCol(cells: Seq[Position]): Int = {
    val cols = (cells map (_._2)).distinct
    if (cols.length == 1)
      cols.head
    else
      0
  }

  def colPeers(cell: Position): Seq[SudokuCell] = column(shareCol(Seq(cell))).filter(_ != field(cell._1)(cell._2))

  //returns index of box that given cell positions share
  // returns zero if given cell positions do not share a box
  def shareBox(cells: Seq[Position]): Int = {
    val colrowindex: Int => Int = (i: Int) => (i - 1) / boxsize
    val boxindices = cells map (p => (colrowindex(p._1) - 1) * boxsize + colrowindex(p._2))
    if (boxindices.length == 1)
      boxindices.head
    else
      0
  }

  def boxPeers(cell: Position): Seq[SudokuCell] = boxelems(shareBox(Seq(cell))).filter(_ != field(cell._1)(cell._2))

  def sharedUnits(cells: Seq[Position]): (Int, Int, Int) =
    (shareRow(cells), shareCol(cells), shareBox(cells))

  def allPeers(cell: Position): Map[Position, SudokuCell] = ???



  /**
    * print just the filled in values
    */
  def toSimpleString(blankstr: String = "0"): String = {
    val s = (for (r <- field) yield (r map (_.value)).mkString("")).mkString("\n")
    if (blankstr != "0")
      s.replace('0', blankstr.head)
    else
      s
  }
}

object SudokuField {

  val cellrange: Range = 1 to 9
  val numregex: String = s"[${cellrange.min}-${cellrange.max}]"
  val blankregex: String = """[0\*\_\.]"""

  val totalcells = cellrange.max * cellrange.max
  val boxsize = Math.sqrt(cellrange.max).toInt

  /**
    * parsing a Sudoku from a String representation;
    * supports two standard format (also supported by sudokuwiki.org):
    * 1) without candidates: 81 digits in a row, where blanks (= unknown values) can be expressed via 0, *, _, or .
    * order of fields is interpreted as left to right, top to bottom
    * example: 4*****938732*941**89531*24*37*6*9**4529**16736*47*3*9*957**83**1*39**4**24**3*7*9
    * or also
    * 4*****938
    * 732*941**
    * 89531*24*
    * 37*6*9**4
    * 529**1673
    * 6*47*3*9*
    * 957**83**
    * 1*39**4**
    * 24**3*7*9
    * any non-digits non-*,_ or ., newline etc. are ignored;
    * if there are more than 81 characters, tries parsing format 2)
    * 2) with candidates, such as
    * +--------------+-----------------+-----------------+
    * |    4  16  16 |   25   257  257 |    9     3    8 |
    * |    7   3   2 |   58     9    4 |    1    56  567 |
    * |    8   9   5 |    3     1   67 |    2     4   67 |
    * +--------------+-----------------+-----------------+
    * |    3   7  18 |    6    25    9 |   58    12    4 |
    * |    5   2   9 |   48    48    1 |    6     7    3 |
    * |    6  18   4 |    7    25    3 |   58     9   12 |
    * +--------------+-----------------+-----------------+
    * |    9   5   7 |  124  1246    8 |    3   126  126 |
    * |    1 168   3 |    9 12567 2567 |    4 12568 1256 |
    * |    2   4 168 |   15     3   56 |    7  1568    9 |
    * +--------------+-----------------+-----------------+
    * parses anything where clues (= known values) and candidate groups (= group of digits in a row) are separated by at least one space
    *
    * @param sfield
    * @return
    */
  def parseStringRepr(sfield: String): Field = {
    val cellregex = s"${numregex}|${blankregex}| ".r
    // try parsing first format
    val filteredstring: String = (cellregex findAllIn (sfield)).mkString("")
    val defaultcandidates = cellrange.toSet
    val transformcell: String => Int = (c: String) => if (c.matches(numregex)) c.toInt else 0
    //transform a sequence of cells to a field, assuming the length is correct already
    val cellstofield: Seq[SudokuCell] => Field = (cells: Seq[SudokuCell]) => cells.sliding(9, 9).map(_.toArray).toArray
    val assigncandidates: (String => Set[Int]) = (s: String) => if (s == "0") defaultcandidates else Set()

    if (filteredstring.length == totalcells) {
      // try parsing format without candidates
      val cells: Seq[SudokuCell] = for (c <- filteredstring) yield SudokuCell(transformcell(c.toString), assigncandidates(c.toString))
      cellstofield(cells)
    } else {
      //try parsing format with candidates
      val cellstrings = filteredstring.split("\\s").filter(s => s.nonEmpty)
      if (cellstrings.length == totalcells) {
        val makecell: String => SudokuCell = (s: String) =>
          if (s.length == 1)
            SudokuCell(transformcell(s), assigncandidates(s))
          else {
            val candidates: Set[Int] = (for (c <- s) yield transformcell(c.toString)).toSet
            SudokuCell(0, candidates)
          }
        val cells: Seq[SudokuCell] = for (c <- cellstrings) yield makecell(c)
        cellstofield(cells)
      } else sys.error("Could not parse given String as Sudoku - not the right format.")
    }
  }
}
