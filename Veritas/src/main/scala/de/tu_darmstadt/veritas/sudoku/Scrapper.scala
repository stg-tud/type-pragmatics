package de.tu_darmstadt.veritas.sudoku

import java.io.{File, PrintWriter}
import java.nio.file.Files

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}

/**
  * Created by andiderp on 17/05/2017.
  */
class Scrapper(numbers: Int, difficulty: Int, numberOfTimes: Int, directory: File) {
  require(numbers == 4 || numbers == 9 || numbers == 16 || numbers == 25)
  require(difficulty >= 0 && difficulty < 6)

  val boxesInRow = math.sqrt(numbers).toInt


  def scrape(): Unit = {
    if (!directory.exists())
      return

    for (i <- 0 until numberOfTimes) {
      val (shown, solution) = getSudoku()
      // write to file
      val sudokuPrefix = s"${i}_${numbers}_${difficulty}"
      writeToFile(new File(directory, s"${sudokuPrefix}_shown"), convertToString(shown))
      writeToFile(new File(directory, s"${sudokuPrefix}_solution"), convertToString(solution))
    }
  }

  private def convertToString(sudoku: Array[Array[String]]): String = {
    val buffer = new StringBuilder("")
    sudoku.foreach { row =>
      buffer.append(row.mkString("")).append("\n")
    }
    buffer.toString
  }

  private def writeToFile(file: File, content: String): Unit = {
    val writer = new PrintWriter(file)
    writer.print(content)
    writer.close()
  }

  /**
    *
    * @return (starting point, solution)
    */
  private def getSudoku(): (Array[Array[String]], Array[Array[String]]) = {
    val site = downloadSite()
    val table = getSudokuTable(site)
    val shownSudoku = new Array[Array[String]](numbers)
    val solutionSudoku = new Array[Array[String]](numbers)

    for (i <- 0 until numbers) {
      shownSudoku(i) = new Array[String](numbers)
      solutionSudoku(i) = new Array[String](numbers)
    }

    for (i <- 0 until numbers;
         j <- 0 until numbers) {
      val (value, solution) = getCell(table)(i, j)
      shownSudoku(i)(j) = value
      solutionSudoku(i)(j) = solution
    }

    (shownSudoku, solutionSudoku)
  }

  private def downloadSite(): Document = {
    val browser = new JsoupBrowser()

    def convertDifficulty(): String = difficulty match {
      case 0 => "Easy"
      case 1 => "Medium"
      case 2 => "Hard"
      case 3 => "MENSA"
      case 4 => "Genius"
      case 5 => "Lex Luthor"
      case _ => "" // cannot happen
    }

    println(boxesInRow)
    val url = "http://sudoku-puzzles.merschat.com/index.cgi"
    val configuredUrl = s"${url}?base=$boxesInRow&type=Letters&diff=${convertDifficulty()}"
    println(configuredUrl)
    browser.get(configuredUrl)
  }

  private def getSudokuTable(doc: Document): Element = {
    val main = grandChild(doc.body)(3)
    val center = child(grandChild(child(main)(1))(2))(3)
    val table = grandChild(child(center)(2))(9)
    table
  }

  private def child(elem: Element)(n: Int): Element = {
    elem.children.toSeq(n)
  }

  private def grandChild(elem: Element)(depth: Int): Element = {
    var curr = elem
    for (i <- 1 to depth)
      curr = curr.children.head
    curr
  }

  /**
    * @param table
    * @param row
    * @param column
    * @return (shown value, solution)
    */
  private def getCell(table: Element)(row: Int, column: Int): (String, String) = {
    val boxRowIndex = row / boxesInRow
    val tableBoxRow = child(table)(boxRowIndex)
    val boxIndex = column / boxesInRow
    val tableBox = grandChild(child(tableBoxRow)(boxIndex))(2)
    val rowInBoxIndex = row - boxesInRow*boxRowIndex
    val tableRowInBox = child(tableBox)(rowInBoxIndex)
    val columnInBoxIndex = column - boxesInRow*boxIndex
    val cell = grandChild(child(tableRowInBox)(columnInBoxIndex))(1)
    val solution = child(cell)(0).attr("value")
    val shownValue = child(cell)(1).attr("value")
    (if (shownValue == "") "." else shownValue, solution)
  }
}

object App {
  def main(agrs: Array[String]): Unit = {
    val scr = new Scrapper(25, 0, 20, new File("test"))
    scr.scrape()
  }
}
