package veritas.benchmarking

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files
import java.util.{Calendar, Date}
import java.util.concurrent.Executors

import veritas.benchmarking.Main.Config
import veritas.benchmarking.util.FileUtil

import scala.collection.parallel.ExecutionContextTaskSupport
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration


case class Runner(config: Config) {

  val summary = new Summary(config)

  def listAllFiles(f: File): Array[File] =
    if (f.isFile && !f.getName.startsWith("."))
      Array(f)
    else if (f.isDirectory && !f.getName.startsWith("."))
      f.listFiles().flatMap(listAllFiles(_))
    else
      Array()

  val allFiles = config.files.flatMap(listAllFiles(_))

  private def extractVeritasConfig(filePath: String): VeritasConfig = {
    // split takes regular expression, \-separator (windows systems) needs to be escaped.
    val fileSeparator =
      if (File.separator == "\\") "\\\\"
      else File.separator
    val pathParts = filePath.split(fileSeparator)
    //assemble configuration from last few parts of path
    val goalcategory = pathParts(pathParts.length - 6)
    val typing = pathParts(pathParts.length - 5)
    val transformations = List(pathParts(pathParts.length - 4), pathParts(pathParts.length - 3), pathParts(pathParts.length - 2))

    new VeritasConfig(goalcategory, typing, transformations)
  }

  private def callProverAndLog: (ProverConfig, File, File) => (VeritasConfFile, FileSummary) = {
    case (proverConfig, file, outfile) => {
      val call = proverConfig.makeCall(file, config.timeout, config.fullLogs)
      val (result, proctime) = Runner.exec(call, config.timeout, config.logExec,
        () => proverConfig.newResultProcessor(outfile, config.timeout))
      val tooltime = result.timeSeconds
      val time = tooltime match {
        case None => proctime
        case Some(t) =>
          val diff = Math.abs(proctime - t)
          if (diff > 0.1)
            println(s"WARNING for prover configuration ${proverConfig.name} and file $file: " +
              s"measured time ${proctime.formatted("%.3f")} " +
              s"differs from tool time $t by ${diff.formatted("%.3f")}.")
          t
      }

      val res = FileSummary(extractVeritasConfig(file.getAbsolutePath), file.getName, proverConfig, result, config.timeout, time)
      val status = res.proverResult.status
      val logDetail = config.logProof && status == Proved ||
        config.logDisproof && status == Disproved ||
        config.logInconclusive && status.isInstanceOf[Inconclusive]
      if (config.logPerFile || logDetail)
        println(s"Prover ${res.proverConfig.name} finished $file in ${res.procTime.formatted("%.3f")} " +
          s"seconds: ${res.proverResult.status}")
      if (logDetail)
        print(res.proverResult.details.toHumanString)


      (VeritasConfFile(extractVeritasConfig(file.getAbsolutePath), file.getName), res)

    }
  }

  private def calcFinishTime(d: Duration): String = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.SECOND, d.toSeconds.toInt)
    cal.getTime.toString
  }

  def executeProvers(): Unit = {
    def makeOutputFile(proverConfig: ProverConfig, file: File): File = {
      val prepath = if (config.logFilePath.isEmpty) "." else config.logFilePath
      val filePath = file.getPath
      val fileSeparator =
        if (File.separator == "\\") "\\\\"
        else File.separator
      val pathParts = filePath.split(fileSeparator)
      val confPath = pathParts.takeRight(5).dropRight(1).mkString("/")
      val filename = file.getName
      val path = s"$prepath/${config.timeout}s/${proverConfig.name}/$confPath/${filename}.proof"
      val filehandler = new File(path)
      if (!filehandler.getParentFile.exists())
        filehandler.getParentFile.mkdirs()
      filehandler.createNewFile()
      filehandler
    }


    val joblist = for {proverConfig <- config.proverConfigs
                       file <- allFiles
                       if (proverConfig.acceptedFileFormats.exists(s => file.getName().endsWith(s)))}
      yield (proverConfig, file, makeOutputFile(proverConfig, file))

    val estimatedDuration = Duration(joblist.length * config.timeout, "seconds")


    // set up parallel calls to provers (careful, resources may differ significantly from sequential case!)
    if (config.parallelism > 0) {
      val parnum = if (config.parallelism == 1)
      //get number of available system CPUs, substract 1 just in case
        Runtime.getRuntime().availableProcessors() - 1
      else config.parallelism

      println(s"Will execute ${joblist.length} jobs, executing $parnum at a time.")
      println(s"Estimated worst case duration: ${estimatedDuration / parnum}, " +
        s"i.e. would be finished on ${calcFinishTime(estimatedDuration / parnum)}")

      val parjoblist = joblist.par
      val threadPool = Executors.newFixedThreadPool(parnum)
      parjoblist.tasksupport =
        new ExecutionContextTaskSupport(ExecutionContext.fromExecutor(threadPool))

      val summarylist = parjoblist.map { case (pc, file, outfile) => callProverAndLog(pc, file, outfile) }
      for (fs <- summarylist.toList) {
        summary += fs
      }
      threadPool.shutdown() //without this line, the worker threads stay alive!
    } else {
      //set up sequential execution!
      println(s"Will execute ${joblist.length} jobs sequentially.")
      println(s"Estimated worst case duration: ${estimatedDuration}, " +
        s"i.e. would be finished on ${calcFinishTime(estimatedDuration)}")
      val summarylist = joblist map { case (pc, file, outfile) => callProverAndLog(pc, file, outfile) }
      for (fs <- summarylist) {
        summary += fs
      }
    }
  }

  def processProofLogs(): Unit = {

    // convention for file names:
    // timeout/proverConfig/goalcategory/typing/variable/optimization/filename.proof
    //
    // override functions below if convention shall be changed!
    def getProverConfig(fp: String): ProverConfig = {
      // split takes regular expression, \-separator (windows systems) needs to be escaped.
      val fileSeparator =
        if (File.separator == "\\") "\\\\"
        else File.separator
      val pathParts = fp.split(fileSeparator)
      val configname = pathParts(pathParts.length - 7)
      //assemble configuration from last two parts of path
      ProverConfig.configs(configname)
    }

    def getFilename(fn: String): String = {
      fn.dropRight(6) //drop .proof
    }

    def getTimeout(fp: String): Int = {
      val fileSeparator =
        if (File.separator == "\\") "\\\\"
        else File.separator
      val pathParts = fp.split(fileSeparator)
      val stimeout = pathParts(pathParts.length - 8)
      val timeout = stimeout.dropRight(1)
      timeout.toInt
    }

    for (file <- allFiles if (file.isFile && file.getName().endsWith(".proof"))) {
      val fn = file.getName()
      val fp = file.getAbsolutePath()
      println(s"Processing $fp ...")
      val inputfile = getFilename(fn)
      val timeout = getTimeout(fp)
      val proverconf = getProverConfig(fp)
      val veritasconf = extractVeritasConfig(fp)

      val resultproc = proverconf.newResultProcessor(file, timeout, true)
      resultproc.processLogs()

      val filesummary = FileSummary(veritasconf, inputfile, proverconf, resultproc.result, timeout, resultproc.result.timeSeconds.getOrElse(0.0))
      resultproc.close()
      summary +=(VeritasConfFile(veritasconf, inputfile), filesummary)
    }
  }

  // produces flat index map of all given proof files
  val flatIndexFileMap: Map[Int, File] = ((1 to allFiles.length) zip allFiles).toMap

  def makeSLURMScripts() = {
    val ssm = SlurmScriptMaker(config.proverConfigs, config.timeout, flatIndexFileMap)
    ssm.writeFlattenedFileStructure()
    ssm.writeJobScripts()
  }

  //sort the indexed output from the HHLR again into
  def sortHHLRProverLogs() = {
    val HHLRFiles = listAllFiles(config.sortHHLRoutput)

    for (hhlr <- HHLRFiles)
      {
        //TODO improve hardcoded numbers?
        val filename = hhlr.getName
        val filepath = hhlr.getParent
        val indexparts = filename.split("_")
        val index = indexparts(1).toInt*1000 + indexparts(2).toInt
        val correspondinginput = flatIndexFileMap(index).getPath
        val fileSeparator =
          if (File.separator == "\\") "\\\\"
          else File.separator
        val splitinputname = correspondinginput.split(fileSeparator)
        val relevantnamepart = splitinputname.takeRight(6).mkString(fileSeparator)
        val destfile = new File(s"$filepath/${relevantnamepart}.proof")

        if (!destfile.getParentFile.exists())
          destfile.getParentFile.mkdirs()

        FileUtil.copyContentOfFile(hhlr, destfile)
        //afterwards, delete HHLR-file
        //flatIndexFileMap(index).delete()

      }

  }


  def run(): Unit = {
    if (config.layoutData.length > 0)
      SingleDataLayout(allFiles, s"${config.timeout}s").layoutAll(config.layoutData)
    else if(config.mergeLayoutData.length > 0)
      MergedBaseDataLayout(allFiles, s"${config.timeout}s").layoutAll(config.mergeLayoutData)
    else if (config.summarizeLogs)
      processProofLogs()
    else if (config.generateSLURM)
      makeSLURMScripts()
    else if (config.sortHHLRoutput != null)
      sortHHLRProverLogs()
    else
      executeProvers()
  }

}

object Runner {
  def exec(call: Seq[String], timeoutSeconds: Int, logExec: Boolean, resultProcessor: () => ResultProcessor): (ProverResult, Double) = {
    import scala.sys.process._

    if (logExec)
      print("Calling " + call.mkString(" ") + " ...")

    val start = System.nanoTime()
    val resultProc = resultProcessor()
    val p = call.run(resultProc)

    import concurrent._
    import ExecutionContext.Implicits.global
    val f = Future(blocking(p.exitValue()))
    try {
      val code = Await.result(f, duration.Duration(timeoutSeconds + 10, "sec"))
      val end = System.nanoTime()

      if (logExec)
        println(" done")

      //process logs once at the end
      resultProc.processLogs()

      (resultProc.result, (end - start).toDouble / 1000000000)

    } catch {
      case _: TimeoutException =>
        if (logExec)
          println()
        p.destroy()
        println(s" *** Broked pipe while calling ${call.head}, restarting prover...")
        exec(call, timeoutSeconds, logExec, resultProcessor)
    }

  }
}
