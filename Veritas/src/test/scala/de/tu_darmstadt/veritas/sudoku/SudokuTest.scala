package de.tu_darmstadt.veritas.sudoku

import java.io.{File, FileReader, FileWriter}

import de.tu_darmstadt.veritas.sudoku.strategies.ApplySingle
import de.tu_darmstadt.veritas.sudoku.tactics.DoNothing
import org.scalatest.FunSuite

import scala.io.Source

/**
  * a couple of simple tests of parsing/creating sudokus
  */
class SudokuTest extends FunSuite {

  // five easy Sudokus
  val s1_no_candidates: String = "4.....938732.941..89531.24.37.6.9..4529..16736.47.3.9.957..83..1.39..4..24..3.7.9"
  val s1_candidates: String =
    """
      +--------------+----------------+---------------+
      |    4  16  16 |   25 2567 2567 |    9    3   8 |
      |    7   3   2 |   58    9    4 |    1   56  56 |
      |    8   9   5 |    3    1   67 |    2    4  67 |
      +--------------+----------------+---------------+
      |    3   7  18 |    6  258    9 |   58 1258   4 |
      |    5   2   9 |   48   48    1 |    6    7   3 |
      |    6  18   4 |    7  258    3 |   58    9 125 |
      +--------------+----------------+---------------+
      |    9   5   7 |  124  246    8 |    3  126 126 |
      |    1  68   3 |    9 2567 2567 |    4 2568 256 |
      |    2   4  68 |   15    3   56 |    7 1568   9 |
      +--------------+----------------+---------------+
    """
  val s1_solution: String = "461572938732894156895316247378629514529481673614753892957248361183967425246135789"

  // has hidden singles
  val s2_no_candidates: String = "...1.5...14....67..8...24...63.7..1.9.......3.1..9.52...72...8..26....35...4.9..."
  val s2_candidates: String =
    """
      +----------------+-------------------+----------------+
      |  2367 379   29 |     1   3468    5 |  2389   9  289 |
      |     1   4  259 |   389     38   38 |     6   7  289 |
      |  3567   8   59 |  3679     36    2 |     4  59   19 |
      +----------------+-------------------+----------------+
      |  2458   6    3 |    58      7   48 |    89   1  489 |
      |     9  57 2458 |   568 124568 1468 |    78  46    3 |
      |   478   1   48 |   368      9 3468 |     5   2 4678 |
      +----------------+-------------------+----------------+
      |   345 359    7 |     2   1356  136 |    19   8 1469 |
      |    48   2    6 |    78     18  178 |   179   3    5 |
      |   358  35  158 |     4  13568    9 |   127   6 1267 |
      +----------------+-------------------+----------------+
    """
  val s2_solution: String = "672145398145983672389762451263574819958621743714398526597236184426817935831459267"

  val s3_no_candidates: String = ".....4.284.6.....51...3.6.....3.1....87...14....7.9.....2.1...39.....5.767.4....."
  val s3_candidates: String =
    """
      +------------------+------------------+-----------------+
      |  357    359  359 |  1569  5679    4 |   379     2   8 |
      |    4    239    6 |  1289  2789  278 |   379  1379   5 |
      |    1    259  589 |  2589     3 2578 |     6    79  49 |
      +------------------+------------------+-----------------+
      |   25  24569  459 |     3 24568    1 |  2789 56789 269 |
      |  235      8    7 |   256   256  256 |     1     4 269 |
      |  235 123456 1345 |     7 24568    9 |   238  3568  26 |
      +------------------+------------------+-----------------+
      |   58     45    2 |  5689     1 5678 |   489   689   3 |
      |    9    134 1348 |   268   268 2368 |     5   168   7 |
      |    6      7 1358 |     4  2589 2358 |   289   189 129 |
      +------------------+------------------+-----------------+
    """
  val s3_solution: String = "735164928426978315198532674249381756387256149561749832852617493914823567673495281"

  //has hidden singles
  val s4_no_candidates: String = ".........9.46.7....768.41..3.97.1.8...8...3...5.3.87.2..75.261....4.32.8........."
  val s4_candidates: String =
    """
      +----------------------+----------------+----------------------+
      |    1258    1238 1235 |  129 12359  59 |  4589 2345679 345679 |
      |       9    1238    4 |    6  1235   7 |    58     235     35 |
      |      25       7    6 |    8  2359   4 |     1    2359    359 |
      +----------------------+----------------+----------------------+
      |       3     246    9 |    7  2456   1 |    45       8    456 |
      |   12467    1246    8 |   29 24569 569 |     3    4569  14569 |
      |     146       5    1 |    3   469   8 |     7     469      2 |
      +----------------------+----------------+----------------------+
      |      48    3489    7 |    5    89   2 |     6       1    349 |
      |     156     169   15 |    4  1679   3 |     2     579      8 |
      |  124568 1234689 1235 |   19 16789  69 |   459   34579  34579 |
      +----------------------+----------------+----------------------+
    """
  val s4_solution: String = "583219467914637825276854139349721586728965341651348792497582613165493278832176954"

  val s5_no_candidates: String = ".32..61..41..........9.1...5...9...4.6.....7.3...2...5...5.8..........19..7...86."
  val s5_candidates: String =
    """
      +------------------+-------------------+---------------------+
      |   789    3     2 |    478  4578    6 |       1  4589    78 |
      |     4    1  5689 |   2378  3578 2357 |  235679 23589 23678 |
      |   678  578   568 |      9 34578    1 |  234567 23458 23678 |
      +------------------+-------------------+---------------------+
      |     5  278    18 |  13678     9   37 |     236   238     4 |
      |  1289    6  1489 |   1348 13458  345 |     239     7  1238 |
      |     3 4789  1489 |  14678     2   47 |      69    89     5 |
      +------------------+-------------------+---------------------+
      |  1269  249 13469 |      5 13467    8 |    2347   234   237 |
      |   268 2458 34568 |  23467  3467 2347 |   23457     1     9 |
      |   129 2459     7 |   1234   134 2349 |       8     6    23 |
      +------------------+-------------------+---------------------+
    """
  val s5_solution: String = "732456198419283756685971423528197634964835271371624985296518347843762519157349862"


  //Sudokus without hidden singles
  val easysudokulist_nc = List(s1_no_candidates, s3_no_candidates, s5_no_candidates)
  val easysudokulist_cand = List(s1_candidates, s3_candidates, s5_candidates)

  implicit val config9: SudokuConfig = SudokuConfig(1 to 9, ".123456789")

  def testNoCandidateParsing(sudoku: String): Unit = {
    val original = new SudokuField(sudoku, config9)
    val parsed = original.toSimpleString()
    //println(parsed)

    assert(parsed.replace("\n", "").replaceAll("0", ".") == sudoku)
  }

  def testCandidateParsing(sudoku_nc: String, sudoku_c: String): Unit = {
    val original = new SudokuField(sudoku_nc, config9)
    val original_with_candidates = new SudokuField(sudoku_c, config9)

    assert(original.toSimpleString() == original_with_candidates.toSimpleString())
  }

  test("Parsing a single string with no candidates yields the expected Sudoku") {
    for (sud <- easysudokulist_nc) testNoCandidateParsing(sud)
  }

  test("Parsing a sudoku string with candidates yields the expected Sudoku") {
    for ((snc, sc) <- (easysudokulist_nc zip easysudokulist_cand)) testCandidateParsing(snc, sc)
  }

  test("Sudoku Proof Graph initialization") {
    val f1 = new SudokuField(s1_candidates, config9)
    val file = new File("SudokuPGStores/Sudoku-1-store")
    file.delete()
    val spg = new SudokuProofGraph(file, f1, new ApplySingle(DoNothing))
    spg.constructPG()
    val storedSudoku = spg.g.findObligation("initial").get.goal.toSimpleString(".")
    assert(storedSudoku.replaceAll("\n", "") == s1_no_candidates)
  }

  test("Sudoku 25x25 Proof Graph initialization") {
    val config25: SudokuConfig = SudokuConfig(1 to 25, ".ABCDEFGHIJKLMNOPQRSTUVWXY")

    val sud25x25 = Source.fromFile("sudokupuzzles/giant25sudoku").getLines().mkString("")
    val field = new SudokuField(sud25x25, config25)
    val store = new File("SudokuPGStores/Sudoku-25-372holes-store")
    store.delete()
    val spg = new SudokuProofGraph(store, field, new ApplySingle(DoNothing))
    spg.constructPG()
    val storedSudoku = spg.g.findObligation("initial").get.goal.toSimpleString(".")
    assert(storedSudoku.replaceAll("\n", "") == sud25x25)
  }

}
