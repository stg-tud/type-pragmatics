package veritas.benchmarking

import java.io.{File, PrintWriter}

import info.folone.scala.poi._
import info.folone.scala.poi.impure.load

import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.util.matching.Regex

case class TransformationError(m: String) extends RuntimeException(m)

abstract class DataLayout(files: Seq[File], timeout: String) {
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

  object SelectionConfEnum extends ConfigOption {
    val Selectall = Value("selectall")
    val Selectusedfp = Value("selectusedfp")
    val Noinversion = Value("noinversion")
    val Noinversionselectusedfp = Value("noinversionselectusedfp")
  }

  trait CSVTransformable {
    def getCSVcells(b: StringBuilder, end: Boolean = false)
  }

  abstract class Key(val proverConf: ProverConfEnum.Value,
                     val goalCategory: GoalCategoryEnum.Value,
                     val typingConf: TypingConfEnum.Value,
                     val variableConf: VariableConfEnum.Value,
                     val simplConf: SimplConfEnum.Value,
                     val selectConf: SelectionConfEnum.Value) extends CSVTransformable {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, proverConf.toString)
      makeCSVcell(b, goalCategory.toString)
      makeCSVcell(b, typingConf.toString)
      makeCSVcell(b, variableConf.toString)
      makeCSVcell(b, simplConf.toString)
      makeCSVcell(b, selectConf.toString, end)
    }
  }

  case class ConfKey(override val proverConf: ProverConfEnum.Value,
                     override val goalCategory: GoalCategoryEnum.Value,
                     override val typingConf: TypingConfEnum.Value,
                     override val variableConf: VariableConfEnum.Value,
                     override val simplConf: SimplConfEnum.Value,
                     override val selectConf: SelectionConfEnum.Value)
    extends Key(proverConf, goalCategory, typingConf, variableConf, simplConf, selectConf)

  case class RawKey(override val proverConf: ProverConfEnum.Value,
                    override val goalCategory: GoalCategoryEnum.Value,
                    override val typingConf: TypingConfEnum.Value,
                    override val variableConf: VariableConfEnum.Value,
                    override val simplConf: SimplConfEnum.Value,
                    override val selectConf: SelectionConfEnum.Value,
                    filename: String) extends Key(proverConf, goalCategory, typingConf, variableConf, simplConf, selectConf) {
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


  protected def makeCSVcell(b: StringBuilder, s: String, end: Boolean = false) = {
    b ++= escape(s, sep, "\"")
    if (end) b ++= "\n"
    else b ++= sep
  }

  val sep = ","

  private def escape(s: String, avoid: String, quote: String): String = {
    if (!s.contains(avoid) && !s.contains("\n"))
      s.trim
    else
      quote + s.trim.replace("\"", "\\\"").replace("\n", "\t\t") + quote
  }

  def extractRawMap(workbook: Workbook) = {
    (for {sh <- workbook.sheets
          row <- sh.rows if (row.index != 0)} yield {
      val pc = extractString(ProverConfEnum, sh.name)
      val gc = extractAtIndex(GoalCategoryEnum, row, 2)
      val tc = extractAtIndex(TypingConfEnum, row, 3)
      val vc = extractAtIndex(VariableConfEnum, row, 4)
      val sc = extractAtIndex(SimplConfEnum, row, 5)
      val selc = extractAtIndex(SelectionConfEnum, row, 6)
      val filename = getCell(row, 7)
      val pt = getCell(row, 8).toDouble
      val stat = makeProverStatus(getCell(row, 9))
      val det = getCell(row, 10)
      RawKey(pc, gc, tc, vc, sc, selc, filename) -> RawResult(pt, stat, det)
    }).toMap
  }


  def extractOverviewMap(workbook: Workbook) = {
    (for {sh <- workbook.sheets
          row <- sh.rows if (row.index != 0)} yield {
      val pc = extractAtIndex(ProverConfEnum, row, 0)
      val gc = extractAtIndex(GoalCategoryEnum, row, 2)
      val tc = extractAtIndex(TypingConfEnum, row, 3)
      val vc = extractAtIndex(VariableConfEnum, row, 4)
      val sc = extractAtIndex(SimplConfEnum, row, 5)
      val selc = extractAtIndex(SelectionConfEnum, row, 6)
      val succnum = getCell(row, 7).toDouble.toInt
      val filenum = getCell(row, 8).toDouble.toInt
      val succrate = getCell(row, 9).toDouble
      val avgsucctime = getCell(row, 10).toDouble
      val avgdev = getCell(row, 11).toDouble
      ConfKey(pc, gc, tc, vc, sc, selc) -> OverviewResult(succnum, filenum, succrate, avgsucctime, avgdev)
    }).toMap
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

  def filterProver[K <: Key, R <: Result](dataMap: K Map R, prover: List[ProverConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (prover contains k.proverConf)) yield (k, v)
  }

  def filterGoalCategory[K <: Key, R <: Result](dataMap: K Map R, goalcats: List[GoalCategoryEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (goalcats contains k.goalCategory)) yield (k, v)
  }

  def filterTypingConf[K <: Key, R <: Result](dataMap: K Map R, typingconf: List[TypingConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (typingconf contains k.typingConf)) yield (k, v)
  }

  def filterVariableConf[K <: Key, R <: Result](dataMap: K Map R, variableconf: List[VariableConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (variableconf contains k.variableConf)) yield (k, v)
  }

  def filterSimplConf[K <: Key, R <: Result](dataMap: K Map R, simplconf: List[SimplConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (simplconf contains k.simplConf)) yield (k, v)
  }

  def filterSelectionConf[K <: Key, R <: Result](dataMap: K Map R, selectConf: List[SelectionConfEnum.Value]): (K Map R) = {
    for ((k, v) <- dataMap if (selectConf contains k.selectConf)) yield (k, v)
  }

  protected def writeToFile(filepath: String, s: String) = {
    val filehandler = new File(filepath)
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(s);
      close
    }
  }

  protected def layoutSuccessRateIndividualOpt[K <: ConfigOption](confopt: K)(accessConfKey: ConfKey => confopt.Value)(filteredoverview: ConfKey Map OverviewResult): String = {
    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val succRateList = (for ((k, v) <- filteredoverview
                               if (accessConfKey(k) == opt)) yield v.succrate).toList
      (opt -> succRateList)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }

  // parameter axsel: true - also append key for axiom selection strategy, false - do not append key for axiom selection strategy
  protected def layoutSuccessRateOfCompStrat(axsel: Boolean)(filteredoverview: ConfKey Map OverviewResult): String = {
    val groupedoverview = filteredoverview.groupBy[String](kr => createShortenedConfCell(kr._1, axsel))
    val intermediateMap: (String Map List[Double]) = for ((cnf, confmap) <- groupedoverview) yield
      cnf -> (confmap.toList map (kr => kr._2.succrate))

    makeCSVColBased(intermediateMap, sortConfsFunction[String])
  }

  // parameter axsel: true - also append key for axiom selection strategy, false - do not append key for axiom selection strategy
  protected def createShortenedConfCell(ck: Key, axsel: Boolean = true): String = {
    val typshort = ck.typingConf match {
      case TypingConfEnum.Barefof => "b"
      case TypingConfEnum.Tff => "t"
      case TypingConfEnum.Guardedfof => "g"
    }

    val varshort = ck.variableConf match {
      case VariableConfEnum.Inlievery => "in"
      case VariableConfEnum.Nameevery => "ne"
      case VariableConfEnum.Namparres => "np"
      case VariableConfEnum.Unchanged => "u"
    }

    val simplshort = ck.simplConf match {
      case SimplConfEnum.Nonsimpl => "n"
      case SimplConfEnum.Logsimpl => "l"
      case SimplConfEnum.Patsimpl => "p"
    }

    val selectshort = if (axsel)
      ck.selectConf match {
        case SelectionConfEnum.Selectall => "a"
        case SelectionConfEnum.Selectusedfp => "u"
        case SelectionConfEnum.Noinversion => "ni"
        case SelectionConfEnum.Noinversionselectusedfp => "niu"
      }
    else ""

    s"$typshort$varshort$simplshort$selectshort"
  }


  //keys of maps designate columns
  protected def makeCSVColBased[K, V](dataMap: (K Map Seq[V]), lt: (K, K) => Boolean): String = {
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

  protected def sortConfsFunction[K] = (k1: K, k2: K) => k1.toString < k2.toString

  class SingleCSVWrapper[K](value: K) extends CSVTransformable {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, value.toString, end)
    }
  }

  def doForProvers[K <: Key, R <: Result](proverlist: List[ProverConfEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R)) = {
    for (prover <- proverlist) {
      val file = s"$filepath/${prover.toString}-$filename"
      val filteredoverview = filterProver(data, List(prover))
      val layouted = layoutfun(filteredoverview)
      if (!layouted.isEmpty) writeToFile(file, layouted)
    }
  }

  def doSingle[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R)) = {
    val file = s"$filepath/$filename"
    val layouted = layoutfun(data)
    if (!layouted.isEmpty) writeToFile(file, layouted)
  }

  def layoutAll(outputPath: String): Unit
}

/**
  * Created by sylvia on 01/03/16.
  *
  * assumes that it receives two excel files:
  * 1) raw data (base name)
  * 2) overview data (base name)
  *
  */

case class SingleDataLayout(files: Seq[File], stimeout: String) extends DataLayout(files, stimeout) {
  val workbooks = for (f <- files) yield load(f.getAbsolutePath)

  val rawworkbook = workbooks.head
  val overviewworkbook = workbooks.last

  val rawMap: RawKey Map RawResult = extractRawMap(rawworkbook)

  val overviewMap: ConfKey Map OverviewResult = extractOverviewMap(overviewworkbook)

  //keys of maps designate rows
  private def makeCSVRowBased[K, V](dataMap: (K Map V), sortValues: ((K, V), (K, V)) => Boolean): String = {
    val b = StringBuilder.newBuilder
    val orderedMap = dataMap.toList.sortWith(sortValues)

    for ((k, v) <- orderedMap) {
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

  def doForCategories[K <: Key, R <: Result](catlist: List[GoalCategoryEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    for (cat <- catlist) {
      val file = s"$filepath/${cat.toString}-$filename"
      val filteredoverview = filterGoalCategory(data, List(cat))
      val layouted = layoutfun(filteredoverview)
      if (!layouted.isEmpty) writeToFile(file, layouted)
    }
  }

  def doForallProvers[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) =
    doForProvers(ProverConfEnum.iterator.toList, filepath, filename, layoutfun, data)

  def doForallCategories[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) =
    doForCategories(GoalCategoryEnum.iterator.toList, filepath, filename, layoutfun, data)

  def doForProversCategories[K <: Key, R <: Result](proverlist: List[ProverConfEnum.Value], catlist: List[GoalCategoryEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    for {prover <- proverlist
         cat <- catlist} {
      val file = s"$filepath/${prover.toString}-${cat.toString}-$filename"
      val filteredoverview = filterGoalCategory(filterProver(data, List(prover)), List(cat))
      val layouted = layoutfun(filteredoverview)
      if (!layouted.isEmpty) writeToFile(file, layouted)
    }
  }

  def doForallProversCategories[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) =
    doForProversCategories(ProverConfEnum.iterator.toList, GoalCategoryEnum.iterator.toList, filepath, filename, layoutfun, data)

  // merges data of the given categories in one table
  def doForProversCategoriesMerge[K <: Key, R <: Result](proverlist: List[ProverConfEnum.Value], catlist: List[GoalCategoryEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    val filtereddata = filterGoalCategory(data, catlist)
    doForProvers(proverlist, filepath, filename, layoutfun, filtereddata)
  }


  private def layoutAvgSuccessTimeIndividualOpt[K <: ConfigOption](confopt: K)(accessConfKey: Key => confopt.Value)(filteredoverview: ConfKey Map OverviewResult): String = {

    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val avgSuccTimeList = (for ((k, v) <- filteredoverview
                                  if (accessConfKey(k) == opt)) yield v.avgSuccTime).toList
      val filterzeroes = avgSuccTimeList filter (p => p != 0.0)
      (opt -> filterzeroes)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }


  private def layoutRawDetailedTime(filteredraw: (RawKey Map RawResult)): String = {

    //transform into triple with (filename, config, time)
    val intermediateList: Iterable[(String, String, Double)] = for ((k, v) <- filteredraw) yield {
      (k.filename, createShortenedConfCell(k), v.provertime)
    }

    val confgrouped = intermediateList.groupBy(_._2) //group by configuration shorthand

    //throw away parts of value that are not needed
    //yields shortconf -> List of provertimes (one for each file)
    val intermediateMap: (String Map List[Double]) = for ((k, v) <- confgrouped) yield
      (k -> (for (t <- v) yield t._3).toList)


    makeCSVColBased(intermediateMap, sortConfsFunction[String])

  }

  private def layoutIndividualSuccessRates(axsel: Boolean)(filteredoverview: (ConfKey Map OverviewResult)): String = {
    val intermediateMap: (String Map Double) = for ((k, v) <- filteredoverview) yield {
      (createShortenedConfCell(k, axsel) -> v.succrate)
    }

    makeCSVRowBased(intermediateMap, (p1: (String, Double), p2: (String, Double)) => p1._2 > p2._2) //sort descending
  }


  def layoutAll(outputPath: String): Unit = {
    println("Layouting!")

    //first, ignore axiom selection (generates data for original PPDP paper)
    val filterselectall = filterSelectionConf(overviewMap, List(SelectionConfEnum.Selectall))


    //Overview graphs all categories for each prover, success rates & average success time
    doForallProvers(s"$outputPath/PerProver/$stimeout/SuccRate", "successrate_per_goalcategory.csv", layoutSuccessRateIndividualOpt(GoalCategoryEnum)(k => k.goalCategory), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_goalcategory.csv", layoutAvgSuccessTimeIndividualOpt(GoalCategoryEnum)(k => k.goalCategory), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(TypingConfEnum)(k => k.typingConf), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(VariableConfEnum)(k => k.variableConf), filterselectall)
    doForallProvers(s"$outputPath/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SimplConfEnum)(k => k.simplConf), filterselectall)

    //Overview graphs per category for each prover, success rates & average success time
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf), filterselectall)
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf), filterselectall)
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), filterselectall)
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(TypingConfEnum)(k => k.typingConf), filterselectall)
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(VariableConfEnum)(k => k.variableConf), filterselectall)
    doForallProversCategories(s"$outputPath/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SimplConfEnum)(k => k.simplConf), filterselectall)


    //TODO: is it sensible to compare encoding strategies *including* the axiom selection domain?

    //Detailed overview (based on raw data): performance of individual conf combinations in each category
    //doForallProversCategories(s"$outputPath/DetailedOverviewPerCat/$stimeout", "time_per_file.csv", layoutRawDetailedTime, rawMap)

    //Detailed layout (based on overview data): success rates per individual conf combination in each category
    doForallProversCategories(s"$outputPath/IndividualConfSuccessRatesPerCat/$stimeout", "individual_conf_succ_rate.csv", layoutIndividualSuccessRates(false), filterselectall)

    //Success rates for individual combinations
    //doSingle(s"$outputPath/PerCompStrat/$stimeout", "stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat) //all provers and all categories together
    doForallProvers(s"$outputPath/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat(false), filterselectall)
    doForallCategories(s"$outputPath/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat(false), filterselectall)
    doForallProversCategories(s"$outputPath/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat(false), filterselectall)


    //val allpbutprincess = List(ProverConfEnum.Vampire_4, ProverConfEnum.Vampire_3, ProverConfEnum.Eprover)
    //val allcatbutexecution = List(GoalCategoryEnum.Synthesis, GoalCategoryEnum.Test, GoalCategoryEnum.Proof, GoalCategoryEnum.Counterexample)

    //layout for paper graph RQ1 (success rate per goal category, all provers)
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/$stimeout/Graph1", "successrate_per_goalcategory.csv", layoutSuccessRateIndividualOpt(GoalCategoryEnum)(k => k.goalCategory), filterselectall)


    //layout for paper graph RQ2 (influence of sort encoding, success rate per sort encoding alternative, all provers except princess, all categories together except execution)
    doForallProvers(
      s"$outputPath/$stimeout/Graph2", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf), filterselectall)

    //layout for paper graph RQ3 (influence of variable encoding, success rate per sort encoding alternative, all provers and all categories)
    doForallProvers(
      s"$outputPath/$stimeout/Graph3", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf), filterselectall)

    //layout for paper graph RQ4 (influence of variable encoding, success rate per sort encoding alternative, all provers except princess, categories: proof + test)
    doForallProvers(
      s"$outputPath/$stimeout/Graph4", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), filterselectall)

    //layout for paper graph RQ5 (influence of simplifications, success rate per simplification alternative, all provers except princess, all categories)
    val filteroutgoodtyping = filterTypingConf(filterselectall, List(TypingConfEnum.Barefof, TypingConfEnum.Tff))
    val filteroutgoodinlining = filterVariableConf(filteroutgoodtyping, List(VariableConfEnum.Inlievery, VariableConfEnum.Unchanged))
    val filtered = filterProver(filteroutgoodinlining, List(ProverConfEnum.Vampire_3, ProverConfEnum.Vampire_4, ProverConfEnum.Eprover))
    doSingle(s"$outputPath/$stimeout/Graph5", "simplificationperformance_allprovers_allcategories.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), filtered)

    //doForProvers(allpbutprincess, s"$outputPath/$stimeout/Graph5", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf))

    //layout for paper graph RQ6 (performance of all comp strategies for all provers and categories together)
    doSingle(s"$outputPath/$stimeout/Graph6", "stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat(false), filterselectall)

    //second, use overviewmap everywhere to compare axiom selection strategies (ignoring the encoding dimensions)
    //Layouts for axiom selection study (new)
    //Overview graphs axiom selection strategies success rates & average success time summarizing all categories, for each prover
    // TODO: is possibly the most interesting graph of all
    // RQ: "Does axiom selection strategy improve success rate? Which one works best?"
    doForallProvers(s"$outputPath/AxiomSelection/PerProver/$stimeout/SuccRate", "successrate_per_axiomselectionconfiguration.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf))
    doForallProvers(s"$outputPath/AxiomSelection/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_axiomselectionconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SelectionConfEnum)(k => k.selectConf))

    //Overview graphs axiom selection strategies per category for each prover, success rates & axiom selection strategies
    doForallProversCategories(s"$outputPath/AxiomSelection/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_axiomselectionconfiguration.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf))
    doForallProversCategories(s"$outputPath/AxiomSelection/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_axiomselectionconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SelectionConfEnum)(k => k.selectConf))

    // TODO: compare axiomselection without bad encoding strategies
    // RQ: "Does axiom selection strategy improve success rate in combination with successful encoding strategies? Which one works best?"
    val filteroutgoodtyping_all = filterTypingConf(overviewMap, List(TypingConfEnum.Barefof, TypingConfEnum.Tff))
    val filteroutgoodinlining_all = filterVariableConf(filteroutgoodtyping_all, List(VariableConfEnum.Inlievery, VariableConfEnum.Unchanged))
    doForallProvers(s"$outputPath/AxiomSelection/PerProverGood/$stimeout/SuccRate", "successrate_per_axiomselectionconfiguration.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf), filteroutgoodinlining_all)
    doForallProvers(s"$outputPath/AxiomSelection/PerProverGood/$stimeout/AvgSuccTime", "avgsuccesstime_per_axiomselectionconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SelectionConfEnum)(k => k.selectConf), filteroutgoodinlining_all)

    // same again, but separately for each goal category
    // RQ: "Does axiom selection strategy improve success rate in combination with successful encoding strategies? Which one works best?"
    doForallProversCategories(s"$outputPath/AxiomSelection/PerProverPerCategoryGood/$stimeout/SuccRate", "successrate_per_axiomselectionconfiguration.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf), filteroutgoodinlining_all)
    doForallProversCategories(s"$outputPath/AxiomSelection/PerProverPerCategoryGood/$stimeout/AvgSuccTime", "avgsuccesstime_per_axiomselectionconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SelectionConfEnum)(k => k.selectConf), filteroutgoodinlining_all)


    // like previous, but do not distinguish between the different provers
    val filteredgoodprovers = filterProver(filteroutgoodinlining_all, List(ProverConfEnum.Vampire_3, ProverConfEnum.Vampire_4, ProverConfEnum.Eprover))
    doSingle(s"$outputPath/AxiomSelection/$stimeout/AllGoodProvers", "selectionperformance_allgoodprovers_allcategories.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf), filteredgoodprovers)

    //compare *each* combination of strategies (including axiom selection strategies)
    doSingle(s"$outputPath/AxiomSelection/$stimeout/OverviewAll", "allstratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat(true), overviewMap)

  }
}

case class MergedBaseDataLayout(files: Seq[File], stimeout: String) extends DataLayout(files, stimeout) {
  val workbooks = for (f <- files) yield load(f.getAbsolutePath)

  val overviews = Seq(workbooks(2), workbooks(3))
  val raws = Seq(workbooks(0), workbooks(1))
  val overviewMaps = overviews map extractOverviewMap _
  val rawMaps = raws map extractRawMap _


  def addOverviewResult(x: OverviewResult, y: OverviewResult): OverviewResult = {
    OverviewResult(x.succnum + y.succnum, x.filenum + y.filenum, (x.succrate + y.succrate) / 2, (x.avgSuccTime + y.avgSuccTime) / 2, (x.avgDev + y.avgDev) / 2)
  }

  def addMapValues(maps: Seq[ConfKey Map OverviewResult]): ConfKey Map OverviewResult = {
    val flattendMap = scala.collection.mutable.Map() ++ maps.head
    for {
      map <- maps.tail
      (k, v) <- map
    } {
      flattendMap(k) = addOverviewResult(flattendMap(k), v)
    }
    Map.empty ++ flattendMap
  }

  case class MergedConfKey(val caseStudy: String,
                           override val proverConf: ProverConfEnum.Value,
                           override val goalCategory: GoalCategoryEnum.Value,
                           override val typingConf: TypingConfEnum.Value,
                           override val variableConf: VariableConfEnum.Value,
                           override val simplConf: SimplConfEnum.Value,
                           override val selectConf: SelectionConfEnum.Value)
    extends Key(proverConf, goalCategory, typingConf, variableConf, simplConf, selectConf) {
    override def getCSVcells(b: StringBuilder, end: Boolean = false) = {
      makeCSVcell(b, caseStudy)
      super.getCSVcells(b, end)
    }
  }

  def mergeMaps(maps: Seq[ConfKey Map OverviewResult]): MergedConfKey Map OverviewResult = {
    val sqlMap = maps(0)
    val qlMap = maps(1)
    val mergedMap = scala.collection.mutable.Map[MergedConfKey, OverviewResult]()

    def addElements(caseStudy: String, map: ConfKey Map OverviewResult): Unit = {
      for ((k, v) <- map) {
        val newKey = MergedConfKey(caseStudy, k.proverConf, k.goalCategory, k.typingConf, k.variableConf, k.simplConf, k.selectConf)
        mergedMap(newKey) = v
      }
    }

    addElements("SQL", sqlMap)
    addElements("QL", qlMap)
    Map.empty ++ mergedMap
  }

  private def layoutSuccessRateIndividualOptMerged[K <: ConfigOption](confopt: K)(accessConfKey: MergedConfKey => confopt.Value)(filteredoverview: MergedConfKey Map OverviewResult): String = {
    val casestudyList = ListBuffer[String]()
    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val succRateList =
        (for ((k, v) <- filteredoverview
              if (accessConfKey(k) == opt)) yield {
          if (confopt.toSeq.head == opt) {
            casestudyList += k.caseStudy
          }
          v.succrate
        }).toList

      (opt -> succRateList)
    }).toMap
    makeCSVColBasedMerged(casestudyList.toSeq, intermediateMap, sortConfsFunction[confopt.Value])
  }

  protected def makeCSVColBasedMerged[K, V](casestudyList: Seq[String], dataMap: (K Map Seq[V]), lt: (K, K) => Boolean): String = {
    val b = StringBuilder.newBuilder
    val orderedkeys = dataMap.keys.toList.sortWith(lt)

    def maxvlength: Int = {
      var max = 0
      for (v <- dataMap.values)
        if ((v.length) > max)
          max = v.length
      max
    }

    def makeCaseStudyRow(i: Int): Unit = {
      if (casestudyList.isDefinedAt(i)) {
        val csvtransformablev = new SingleCSVWrapper[String](casestudyList(i))
        csvtransformablev.getCSVcells(b, false)
      } else
        makeCSVcell(b, "", false)
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
    val csvtransformablek = new SingleCSVWrapper[String]("casestudy")
    csvtransformablek.getCSVcells(b, false)
    for (k <- orderedkeys) {
      val csvtransformablek = new SingleCSVWrapper[K](k)
      val last = (k == orderedkeys.last)
      csvtransformablek.getCSVcells(b, last)
    }

    //attach value rows
    for (i <- 0 until maxvlength) {
      makeCaseStudyRow(i)
      makeRow(i)
    }

    b.toString()
  }

  def layoutAll(outputPath: String): Unit = {
    val filterselectall = overviewMaps map {
      filterSelectionConf(_, List(SelectionConfEnum.Selectall))
    }
    val addedfilterselectall = addMapValues(filterselectall)
    val mergedfilterselectall = mergeMaps(filterselectall)

    // RQ1 merged from QL and SQL
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/$stimeout/Graph1", "successrate_per_goalcategory.csv", layoutSuccessRateIndividualOpt(GoalCategoryEnum)(k => k.goalCategory), addedfilterselectall)
    // RQ2 merged from QL and SQL but indicating to which case study values belong
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/$stimeout/Graph2", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOptMerged(TypingConfEnum)(k => k.typingConf), mergedfilterselectall)
    // RQ3 merged from QL and SQL
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/$stimeout/Graph3", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf), addedfilterselectall)
    // RQ4 merged from QL and SQL but indicating to which case study values belong
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/$stimeout/Graph4", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOptMerged(SimplConfEnum)(k => k.simplConf), mergedfilterselectall)
    // RQ5 merged from QL and SQL
    val filteroutgoodtyping = filterTypingConf(addedfilterselectall, List(TypingConfEnum.Barefof, TypingConfEnum.Tff))
    val filteroutgoodinlining = filterVariableConf(filteroutgoodtyping, List(VariableConfEnum.Inlievery, VariableConfEnum.Unchanged))
    val filtered = filterProver(filteroutgoodinlining, List(ProverConfEnum.Vampire_3, ProverConfEnum.Vampire_4, ProverConfEnum.Eprover))
    doSingle(s"$outputPath/$stimeout/Graph5", "simplificationperformance_allprovers_allcategories.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), addedfilterselectall)

    //layout for paper graph RQ6 (performance of all comp strategies for all provers and categories together)
    val filterselectallsql = filterselectall(0)
    val filterselectallql = filterselectall(1)
    doSingle(s"$outputPath/$stimeout/Graph6", "sql_stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat(false), filterselectallsql)
    doSingle(s"$outputPath/$stimeout/Graph6", "ql_stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat(false), filterselectallql)

    // RQ: "Does axiom selection strategy improve success rate? Which one works best?"
    val addedMaps = addMapValues(overviewMaps)
    doForProvers(ProverConfEnum.iterator.toList,
      s"$outputPath/AxiomSelection/PerProver/$stimeout/SuccRate", "successrate_per_axiomselectionconfiguration.csv", layoutSuccessRateIndividualOpt(SelectionConfEnum)(k => k.selectConf), addedMaps)
  }
}