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
  * 1) raw data (base name)
  * 2) overview data (base name)
  *
  */
case class DataLayout(files: Seq[File], stimeout: String) {

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

  private def writeToFile(filepath: String, s: String) = {
    val filehandler = new File(filepath)
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(s);
      close
    }
  }

  def doSingle[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    val file = s"$filepath/$filename"
    writeToFile(file, layoutfun(data))
  }

  def doForProvers[K <: Key, R <: Result](proverlist: List[ProverConfEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    for (prover <- proverlist) {
      val file = s"$filepath/${prover.toString}-$filename"
      val filteredoverview = filterProver(data, List(prover))
      writeToFile(file, layoutfun(filteredoverview))
    }
  }

  def doForCategories[K <: Key, R <: Result](catlist: List[GoalCategoryEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    for (cat <- catlist) {
      val file = s"$filepath/${cat.toString}-$filename"
      val filteredoverview = filterGoalCategory(data, List(cat))
      writeToFile(file, layoutfun(filteredoverview))
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
      writeToFile(file, layoutfun(filteredoverview))
    }
  }

  def doForallProversCategories[K <: Key, R <: Result](filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) =
    doForProversCategories(ProverConfEnum.iterator.toList, GoalCategoryEnum.iterator.toList, filepath, filename, layoutfun, data)

  // merges data of the given categories in one table
  def doForProversCategoriesMerge[K <: Key, R <: Result](proverlist: List[ProverConfEnum.Value], catlist: List[GoalCategoryEnum.Value], filepath: String, filename: String, layoutfun: (K Map R) => String, data: (K Map R) = overviewMap) = {
    val filtereddata = filterGoalCategory(data, catlist)
    doForProvers(proverlist, filepath, filename, layoutfun, filtereddata)
  }

  private def sortConfsFunction[K] = (k1: K, k2: K) => k1.toString < k2.toString

  private def layoutSuccessRateIndividualOpt[K <: ConfigOption](confopt: K)(accessConfKey: ConfKey => confopt.Value)(filteredoverview: ConfKey Map OverviewResult) : String = {

    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val succRateList = (for ((k, v) <- filteredoverview
                               if (accessConfKey(k) == opt)) yield v.succrate).toList
      (opt -> succRateList)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }


  private def layoutSuccessRateOfCompStrat(filteredoverview: ConfKey Map OverviewResult) : String = {
    val groupedoverview = filteredoverview.groupBy[String](kr => createShortenedConfCell(kr._1))
    val intermediateMap: (String Map List[Double]) = for ((cnf, confmap) <- groupedoverview) yield
      cnf -> (confmap.toList map (kr => kr._2.succrate))

    makeCSVColBased(intermediateMap, sortConfsFunction[String])
  }


  private def layoutAvgSuccessTimeIndividualOpt[K <: ConfigOption](confopt: K)(accessConfKey: Key => confopt.Value)(filteredoverview: ConfKey Map OverviewResult) : String = {

    val intermediateMap: (confopt.Value Map List[Double]) = (for (opt <- confopt.iterator) yield {
      val avgSuccTimeList = (for ((k, v) <- filteredoverview
                                  if (accessConfKey(k) == opt)) yield v.avgSuccTime).toList
      val filterzeroes = avgSuccTimeList filter (p => p != 0.0)
      (opt -> filterzeroes)
    }).toMap

    makeCSVColBased(intermediateMap, sortConfsFunction[confopt.Value])
  }

  private def createShortenedConfCell(ck: Key): String = {
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

    s"$typshort$varshort$simplshort"
  }

  private def layoutRawDetailedTime(filteredraw: (RawKey Map RawResult)) : String = {

    //transform into triple with (filename, config, time)
    val intermediateList: Iterable[(String, String, Double)] = for ((k, v) <- filteredraw) yield {
      (k.filename, createShortenedConfCell(k), v.provertime)
    }

    val confgrouped = intermediateList.groupBy(_._2) //group by configuration shorthand

    //throw away parts of value that are not needed
    //yields shortconf -> List of provertimes (one for each file)
    val intermediateMap: (String Map List[Double]) = for ((k,v) <- confgrouped) yield
      (k -> (for (t <- v) yield t._3).toList)


    makeCSVColBased(intermediateMap, sortConfsFunction[String])

  }

  private def layoutIndividualSuccessRates(filteredoverview: (ConfKey Map OverviewResult)) : String = {
    val intermediateMap: (String Map Double) = for ((k, v) <- filteredoverview) yield {
      (createShortenedConfCell(k) -> v.succrate)
    }

    makeCSVRowBased(intermediateMap, (p1 : (String, Double), p2: (String, Double)) => p1._2 > p2._2) //sort descending
  }


  def layoutAll(): Unit = {
    println("Layouting!")

    //Overview graphs all categories for each prover, success rates & average success time
    doForallProvers(s"datasets/layout/PerProver/$stimeout/SuccRate", "successrate_per_goalcategory.csv", layoutSuccessRateIndividualOpt(GoalCategoryEnum)(k => k.goalCategory))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_goalcategory.csv", layoutAvgSuccessTimeIndividualOpt(GoalCategoryEnum)(k => k.goalCategory))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(TypingConfEnum)(k => k.typingConf))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(VariableConfEnum)(k => k.variableConf))
    doForallProvers(s"datasets/layout/PerProver/$stimeout/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SimplConfEnum)(k => k.simplConf))

    //Overview graphs per category for each prover, success rates & average success time
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf))
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf))
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/SuccRate", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf))
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_typingconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(TypingConfEnum)(k => k.typingConf))
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_variableconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(VariableConfEnum)(k => k.variableConf))
    doForallProversCategories(s"datasets/layout/PerProverPerCategory/$stimeout/AvgSuccTime", "avgsuccesstime_per_simplificationconfiguration.csv", layoutAvgSuccessTimeIndividualOpt(SimplConfEnum)(k => k.simplConf))

    //Detailed overview (based on raw data): performance of individual conf combinations in each category
    doForallProversCategories(s"datasets/layout/DetailedOverviewPerCat/$stimeout", "time_per_file.csv", layoutRawDetailedTime, rawMap)

    //Detailed layout (based on overview data): success rates per individual conf combination in each category
    doForallProversCategories(s"datasets/layout/IndividualConfSuccessRatesPerCat/$stimeout", "individual_conf_succ_rate.csv", layoutIndividualSuccessRates)

    //Success rates for individual combinations
    doSingle(s"datasets/layout/PerCompStrat/$stimeout", "stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat) //all provers and all categories together
    doForallProvers(s"datasets/layout/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat)
    doForallCategories(s"datasets/layout/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat)
    doForallProversCategories(s"datasets/layout/PerCompStrat/$stimeout", "stratperformance.csv", layoutSuccessRateOfCompStrat)


    val allpbutprincess = List(ProverConfEnum.Vampire_4, ProverConfEnum.Vampire_3, ProverConfEnum.Eprover)
    val allcatbutexecution = List(GoalCategoryEnum.Synthesis, GoalCategoryEnum.Test, GoalCategoryEnum.Proof, GoalCategoryEnum.Counterexample)

    //layout for paper graph RQ1 (success rate per goal category, all provers)
    doForProvers(ProverConfEnum.iterator.toList,
      s"datasets/layout/$stimeout/Graph1", "successrate_per_goalcategory.csv", layoutSuccessRateIndividualOpt(GoalCategoryEnum)(k => k.goalCategory))

    //layout for paper graph RQ2 (performance of all comp strategies for all provers and categories together)
    doSingle(s"datasets/layout/$stimeout/Graph2", "stratperformance_allprovers_allcategories.csv", layoutSuccessRateOfCompStrat)

    //layout for paper graph RQ3 (influence of sort encoding, success rate per sort encoding alternative, all provers except princess, all categories together except execution)
    doForallProvers(
      s"datasets/layout/$stimeout/Graph3", "successrate_per_typingconfiguration.csv", layoutSuccessRateIndividualOpt(TypingConfEnum)(k => k.typingConf))

    //layout for paper graph RQ4 (influence of variable encoding, success rate per sort encoding alternative, all provers and all categories)
    doForallProvers(
      s"datasets/layout/$stimeout/Graph4", "successrate_per_variableconfiguration.csv", layoutSuccessRateIndividualOpt(VariableConfEnum)(k => k.variableConf))

    //layout for paper graph RQ5 (influence of variable encoding, success rate per sort encoding alternative, all provers except princess, categories: proof + test)
    doForallProvers(
      s"datasets/layout/$stimeout/Graph5", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf))

    //layout for paper graph 5 (influence of simplifications, success rate per simplification alternative, all provers except princess, all categories)
    val filteroutgoodtyping = filterTypingConf(overviewMap, List(TypingConfEnum.Barefof, TypingConfEnum.Tff))
    val filteroutgoodinlining = filterVariableConf(filteroutgoodtyping, List(VariableConfEnum.Inlievery, VariableConfEnum.Unchanged))
    val filtered = filterProver(filteroutgoodinlining, List(ProverConfEnum.Vampire_3, ProverConfEnum.Vampire_4, ProverConfEnum.Eprover))
    doSingle(s"datasets/layout/$stimeout/Graph6", "simplificationperformance_allprovers_allcategories.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf), filtered)

    //doForProvers(allpbutprincess, s"datasets/layout/$stimeout/Graph5", "successrate_per_simplificationconfiguration.csv", layoutSuccessRateIndividualOpt(SimplConfEnum)(k => k.simplConf))

  }
}
