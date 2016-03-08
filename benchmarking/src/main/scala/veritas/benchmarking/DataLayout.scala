package veritas.benchmarking

import java.io.{PrintWriter, File}
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
    val Vampire_3 = Value("vampire-3.0")
    val Vampire_4 = Value("vampire-4.0")
    val Eprover = Value("eprover")
    val Princess_Casc = Value("princess")
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

  val sep = ","

  private def escape(s: String, avoid: String, quote: String): String = {
    if (!s.contains(avoid) && !s.contains("\n"))
      s.trim
    else
      quote + s.trim.replace("\"", "\\\"").replace("\n", "\t\t") + quote
  }

  private def makeCSVcell(b: StringBuilder, s: String, end: Boolean = false) = {
    b ++= escape(s, sep, "\"")
    if (end) b ++= "\n"
    else b ++= sep
  }

  trait CSVTransformable {
    def getCSVcells(b: StringBuilder, end: Boolean = false)
  }

  abstract class Key(val proverConf: ProverConfEnum.Value,
                     val goalCategory: GoalCategoryEnum.Value,
                     val typingConf: TypingConfEnum.Value,
                     val variableConf: VariableConfEnum.Value,
                     val simplConf: SimplConfEnum.Value) extends CSVTransformable {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, proverConf.toString)
      makeCSVcell(b, goalCategory.toString)
      makeCSVcell(b, typingConf.toString)
      makeCSVcell(b, variableConf.toString)
      makeCSVcell(b, simplConf.toString, end)
    }
  }

  case class ConfKey(override val proverConf: ProverConfEnum.Value,
                     override val goalCategory: GoalCategoryEnum.Value,
                     override val typingConf: TypingConfEnum.Value,
                     override val variableConf: VariableConfEnum.Value,
                     override val simplConf: SimplConfEnum.Value)
    extends Key(proverConf, goalCategory, typingConf, variableConf, simplConf)

  case class RawKey(override val proverConf: ProverConfEnum.Value,
                    override val goalCategory: GoalCategoryEnum.Value,
                    override val typingConf: TypingConfEnum.Value,
                    override val variableConf: VariableConfEnum.Value,
                    override val simplConf: SimplConfEnum.Value,
                    filename: String) extends Key(proverConf, goalCategory, typingConf, variableConf, simplConf) {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      super.getCSVcells(b)
      makeCSVcell(b, filename, end)
    }
  }

  abstract class Result extends CSVTransformable

  case class OverviewResult(succnum: Int, filenum: Int, succrate: Double, avgSuccTime: Double, avgDev: Double) extends Result {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, succnum.toString)
      makeCSVcell(b, filenum.toString)
      makeCSVcell(b, succrate.toString)
      makeCSVcell(b, avgSuccTime.toString, end)
      makeCSVcell(b, avgDev.toString)
    }
  }

  case class RawResult(provertime: Double, status: ProverStatus, details: String) extends Result {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, provertime.toString)
      makeCSVcell(b, status.toString)
      makeCSVcell(b, details)
    }
  }


  val workbooks = for (f <- files) yield load(f.getAbsolutePath)

  val rawworkbook = workbooks.head
  val overviewworkbook = workbooks.last

  val rawMap: RawKey Map RawResult = extractRawMap()

  val overviewMap: ConfKey Map OverviewResult = extractOverviewMap()

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
    (for {sh <- overviewworkbook.sheets
          row <- sh.rows if (row.index != 0)} yield {
      val pc = extractAtIndex(ProverConfEnum, row, 0)
      val gc = extractAtIndex(GoalCategoryEnum, row, 2)
      val tc = extractAtIndex(TypingConfEnum, row, 3)
      val vc = extractAtIndex(VariableConfEnum, row, 4)
      val sc = extractAtIndex(SimplConfEnum, row, 5)
      val succnum = getCell(row, 6).toDouble.toInt
      val filenum = getCell(row, 7).toDouble.toInt
      val succrate = getCell(row, 8).toDouble
      val avgsucctime = getCell(row, 9).toDouble
      val avgdev = getCell(row, 10).toDouble
      ConfKey(pc, gc, tc, vc, sc) -> OverviewResult(succnum, filenum, succrate, avgsucctime, avgdev)
    }).toMap
  }

  class SingleCSVWrapper[K](value: K) extends CSVTransformable {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, value.toString, end)
    }
  }

  //keys of maps designate rows
  private def makeCSVRowBased[K, V](dataMap: (K Map V)): String = {
    val b = StringBuilder.newBuilder

    for ((k, v) <- dataMap) {
      val csvtransformablek =
        if (k.isInstanceOf[CSVTransformable])
          k.asInstanceOf[CSVTransformable]
        else new SingleCSVWrapper[K](k)
      val csvtransformablev =
        if (v.isInstanceOf[CSVTransformable])
          v.asInstanceOf[CSVTransformable]
        else new SingleCSVWrapper[V](v)
      csvtransformablek.getCSVcells(b)
      csvtransformablev.getCSVcells(b, true)
    }

    b.toString()
  }

  //keys of maps designate columns
  private def makeCSVColBased[K, V](dataMap: (K Map Seq[V]), lt: (K, K) => Boolean): String = {
    val b = StringBuilder.newBuilder
    val orderedkeys = dataMap.keys.toList.sortWith(lt)
    def maxvlength: Int = {
      var max = 0
      for (v <- dataMap.values)
        if ((v.length) > max)
          max = v.length
      max
    }

    def makeRow(i: Int): Unit = {
      for (k <- orderedkeys) {
        val last = (k == orderedkeys.last)
        if (dataMap(k).isDefinedAt(i)) {
          val csvtransformablev = new SingleCSVWrapper[V](dataMap(k)(i))
          csvtransformablev.getCSVcells(b, last)
        } else //make an empty cell
          makeCSVcell(b, "", last)
      }
    }

    //make key row first
    for (k <- orderedkeys) {
      val csvtransformablek = new SingleCSVWrapper[K](k)
      val last = (k == orderedkeys.last)
      csvtransformablek.getCSVcells(b, last)
    }

    //attach value rows
    for (i <- 0 until maxvlength) {
      makeRow(i)
    }

    b.toString()
  }

  def filterProver[K <: ConfKey, R <: Result](dataMap: K Map R, prover: List[ProverConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (prover contains k.proverConf)) yield (k, v)
  }

  def filterGoalCategory[K <: ConfKey, R <: Result](dataMap: K Map R, goalcats: List[GoalCategoryEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (goalcats contains k.goalCategory)) yield (k, v)
  }

  def filterTypingConf[K <: ConfKey, R <: Result](dataMap: K Map R, typingconf: List[TypingConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (typingconf contains k.typingConf)) yield (k, v)
  }

  def filterVariableConf[K <: ConfKey, R <: Result](dataMap: K Map R, variableconf: List[VariableConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (variableconf contains k.variableConf)) yield (k, v)
  }

  def filterSimplConf[K <: ConfKey, R <: Result](dataMap: K Map R, simplconf: List[SimplConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (simplconf contains k.simplConf)) yield (k, v)
  }

  private def writeToFile(filepath: String, s: String) =
    new PrintWriter(filepath) {
      write(s);
      close
    }

  def doForallProvers(filepath: String, filename: String, layoutfun: (ConfKey Map OverviewResult) => String) = {
    for (prover <- ProverConfEnum.iterator) {
      val file = s"$filepath/${prover.toString}-$filename"
      val filteredoverview = filterProver(overviewMap, List(prover))
      writeToFile(file, layoutfun(filteredoverview))
    }
  }

  def doForallProversCategories(filepath: String, filename: String, layoutfun: (ConfKey Map OverviewResult) => String) = {
    for {prover <- ProverConfEnum.iterator
        cat <- GoalCategoryEnum.iterator} {
      val file = s"$filepath/${prover.toString}-${cat.toString}-$filename"
      val filteredoverview = filterGoalCategory(filterProver(overviewMap, List(prover)), List(cat))
      writeToFile(file, layoutfun(filteredoverview))
    }
  }

  private def sortConfsFunction[K] = (k1: K, k2: K) => k1.toString < k2.toString

  private def layoutSuccessRate[K <: ConfigOption](confopt: K)(accessConfKey: ConfKey => confopt.Value)(filteredoverview: ConfKey Map OverviewResult) : String = {
    //val filteredoverview = filterProver(overviewMap, List(prover))

    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val succRateList = (for ((k, v) <- filteredoverview
                               if (accessConfKey(k) == opt)) yield v.succrate).toList
      (opt -> succRateList)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }


  def layoutAvgSuccessTime[K <: ConfigOption](confopt: K)(accessConfKey: ConfKey => confopt.Value)(filteredoverview: ConfKey Map OverviewResult) : String = {
    //val filteredoverview = filterProver(overviewMap, List(prover))

    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val avgSuccTimeList = (for ((k, v) <- filteredoverview
                                  if (accessConfKey(k) == opt)) yield v.avgSuccTime).toList
      val filterzeroes = avgSuccTimeList filter (p => p != 0.0)
      (opt -> filterzeroes)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }


  def layoutAll(): Unit = {
    println("Layouting!")
    doForallProvers("datasets/layout/PerProver/SuccRate", "successrate_per_goalcategory.csv", layoutSuccessRate(GoalCategoryEnum)(k => k.goalCategory))
    doForallProvers("datasets/layout/PerProver/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRate(TypingConfEnum)(k => k.typingConf))
    doForallProvers("datasets/layout/PerProver/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRate(VariableConfEnum)(k => k.variableConf))
    doForallProvers("datasets/layout/PerProver/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRate(SimplConfEnum)(k => k.simplConf))
    doForallProvers("datasets/layout/PerProver/AvgSuccTime", "avgsuccesstime_per_goalcategory.csv", layoutAvgSuccessTime(GoalCategoryEnum)(k => k.goalCategory))
    doForallProvers("datasets/layout/PerProver/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTime(TypingConfEnum)(k => k.typingConf))
    doForallProvers("datasets/layout/PerProver/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTime(VariableConfEnum)(k => k.variableConf))
    doForallProvers("datasets/layout/PerProver/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTime(SimplConfEnum)(k => k.simplConf))

    doForallProversCategories("datasets/layout/PerProverPerCategory/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRate(TypingConfEnum)(k => k.typingConf))
    doForallProversCategories("datasets/layout/PerProverPerCategory/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRate(VariableConfEnum)(k => k.variableConf))
    doForallProversCategories("datasets/layout/PerProverPerCategory/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRate(SimplConfEnum)(k => k.simplConf))
    doForallProversCategories("datasets/layout/PerProverPerCategory/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTime(TypingConfEnum)(k => k.typingConf))
    doForallProversCategories("datasets/layout/PerProverPerCategory/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTime(VariableConfEnum)(k => k.variableConf))
    doForallProversCategories("datasets/layout/PerProverPerCategory/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTime(SimplConfEnum)(k => k.simplConf))

  }
}
