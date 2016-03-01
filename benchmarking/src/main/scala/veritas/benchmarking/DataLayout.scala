package veritas.benchmarking

import java.io.File
import java.util.NoSuchElementException
import info.folone.scala.poi._
import info.folone.scala.poi.impure.load

import scala.reflect.NameTransformer
import scala.util.matching.Regex


/**
  * Custom exception
  * @param m message
  */
case class TransformationError(m: String) extends RuntimeException(m)

/**
  * Created by sylvia on 01/03/16.
  *
  * assumes that it receives two excel files:
  * 1) raw data
  * 2) overview data
  *
  */
case class DataLayout(files: Seq[File]) {

  type ConfigValue = Any

  trait ConfigOption extends Enumeration with Iterable[ConfigValue] {
    override def iterator = values.iterator

    override def toString =
      ((getClass.getName stripSuffix NameTransformer.MODULE_SUFFIX_STRING split '.').last split
        Regex.quote(NameTransformer.NAME_JOIN_STRING)).last
  }

  object ProverConfEnum extends ConfigOption {
    val Vampire_3 = Value("vampire-proof-3.0")
    val Vampire_4 = Value("vampire-proof-4.0")
    val Eprover = Value("eprover")
    val Princess_Casc = Value("princess-casc")
  }

  object GoalCategoryEnum extends ConfigOption {
    val Counterexample = Value("counterexample")
    val Execution = Value("execution")
    val Proof = Value("proof")
    val Synthesis = Value("synthesis")
    val Test = Value("test")
  }

  object TypingConfEnum extends ConfigOption {
    val Barefof = Value("barefof")
    val Guardedfof = Value("guardedfof")
    val Tff = Value("tff")
  }

  object VariableConfEnum extends ConfigOption {
    val Unchanged = Value("unchanged")
    val Inlievery = Value("inlievery")
    val Nameevery = Value("nameevery")
    val Namparres = Value("namparres")
  }

  object SimplConfEnum extends ConfigOption {
    val Nonsimpl = Value("nonsimpl")
    val Logsimpl = Value("logsimpl")
    val Patsimpl = Value("patsimpl")
  }

  //Parametric class for translating a given String to a Configuration value such as the ones above
  case class ConfValueExtractor[E <: ConfigOption](val e: E) {

    def extractConfVal(s: String): e.Value = {
      try {
        (for (v <- e.values if (v.toString == s)) yield v).head
      } catch {
        case e: NoSuchElementException => throw TransformationError(s"$s was not of type $e")
        case e: Exception => throw e
      }
    }
  }

  private def makeProverStatus(s: String): ProverStatus = {
    val stat = """([a-zA-Z]+)([\\(.+\\)]?)""".r.unanchored
    s match {
      case stat(ps, det) if (ps == "Proved") => Proved
      case stat(ps, det) if (ps == "Disproved") => Disproved
      case stat(ps, det) if (ps == "Inconclusive") => Inconclusive(det)
    }
  }

  private def extractString[E <: ConfigOption](e: E, s: String) = ConfValueExtractor(e).extractConfVal(s)

  private def getCell(row: Row, i: Int): String = row.cells.find(c => c.index == i).getOrElse(null) match {
    case StringCell(i, s) => s
    case NumericCell(i, n) => n.toString
    case c => c.toString()
  }

  private def extractAtIndex[E <: ConfigOption](e: E, row: Row, i: Int) = extractString(e, getCell(row, i))

  case class RawKey(proverConf: ProverConfEnum.Value,
                    goalCategory: GoalCategoryEnum.Value,
                    typingConf: TypingConfEnum.Value,
                    variableConf: VariableConfEnum.Value,
                    simplConfEnum: SimplConfEnum.Value,
                    filename: String)

  case class RawResult(provertime: Double, status: ProverStatus, details: String)

  case class OverviewKey(proverConf: ProverConfEnum.Value,
                         goalCategory: GoalCategoryEnum.Value,
                         typingConf: TypingConfEnum.Value,
                         variableConf: VariableConfEnum.Value,
                         simplConfEnum: SimplConfEnum.Value)

  case class OverviewResult(succnum: Int, filenum: Int, succrate: Double, avgSuccTime: Double, avgDev: Double)

  val workbooks = for (f <- files) yield load(f.getAbsolutePath)

  val rawworkbook = workbooks.head
  val overviewworkbook = workbooks.last

  val rawMap: RawKey Map RawResult = extractRawMap()

  val overviewMap: OverviewKey Map OverviewResult = extractOverviewMap()

  def extractRawMap() = {
    (for {sh <- rawworkbook.sheets
          row <- sh.rows if (row.index != 0)} yield {
      val pc = extractString(ProverConfEnum, sh.name)
      val gc = extractAtIndex(GoalCategoryEnum, row, 2)
      val tc = extractAtIndex(TypingConfEnum, row, 3)
      val vc = extractAtIndex(VariableConfEnum, row, 4)
      val sc = extractAtIndex(SimplConfEnum, row, 5)
      val filename = getCell(row, 6)
      val pt = getCell(row, 7).toDouble
      val stat = makeProverStatus(getCell(row, 8))
      val det = getCell(row, 9)
      RawKey(pc, gc, tc, vc, sc, filename) -> RawResult(pt, stat, det)
    }).toMap
  }


  def extractOverviewMap() = {
    (for {sh <- rawworkbook.sheets
          row <- sh.rows if (row.index != 0)} yield {
      val pc = extractAtIndex(ProverConfEnum, row, 0)
      val gc = extractAtIndex(GoalCategoryEnum, row, 2)
      val tc = extractAtIndex(TypingConfEnum, row, 3)
      val vc = extractAtIndex(VariableConfEnum, row, 4)
      val sc = extractAtIndex(SimplConfEnum, row, 5)
      val succnum = getCell(row, 7).toInt
      val filenum = getCell(row, 8).toInt
      val succrate = getCell(row, 9).toDouble
      val avgsucctime = getCell(row, 10).toDouble
      val avgdev = getCell(row, 11).toDouble
      OverviewKey(pc, gc, tc, vc, sc) -> OverviewResult(succnum, filenum, succrate, avgsucctime, avgdev)
    }).toMap
  }


  def layoutAll(): Unit =
    println("Layouting!")
}
