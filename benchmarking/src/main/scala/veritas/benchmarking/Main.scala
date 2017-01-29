package veritas.benchmarking

import java.io.{FilenameFilter, PrintWriter, PrintStream, File}

import scopt.OptionParser

object Main extends App {

  case class Config(
                     files: Seq[File] = Seq(),
                     repetitions: Int = 1,
                     timeout: Double = 0,
                     proverConfigs: Seq[ProverConfig] = Seq(),
                     fullLogs: Boolean = false,
                     logExec: Boolean = false,
                     logPerFile: Boolean = false,
                     logProof: Boolean = false,
                     logDisproof: Boolean = false,
                     logInconclusive: Boolean = false,
                     logSummary: Boolean = false,
                     logCSV: File = null,
                     logXLS: File = null,
                     logXLSOverview: File = null,
                     parallelism: Int = 0,
                     logFilePath: String = "",
                     summarizeLogs: Boolean = false,
                     layoutData: Boolean = false,
                     generateSLURM: Boolean = false,
                     sortHHLRoutput: File = null
                   ) {
    def ensureDefaultOptions: Config = {
      if (!logExec && !logPerFile && !logProof && !logDisproof && !logInconclusive && !logSummary && logCSV == null &&
        logXLS == null && !summarizeLogs && !layoutData && !generateSLURM && sortHHLRoutput == null)
        copy(logPerFile = true, logSummary = true)
      else
        this
    }
  }

  val optionParser = new OptionParser[Config]("veritas-benchmarking") {
    head("veritas-benchmarking")

    //    opt[Int]("repeat") action { (n, config) =>
    //      config.copy(repetitions = n)
    //    } text("number of repetitions per proof")

    opt[Int]('t', "timeout") validate { t =>
      if (t > 0) success else failure(s"requires positive timeout")
    } action { (t, config) =>
      config.copy(timeout = t)
    }

    val a = this


    opt[String]('c', "config") unbounded() validate { c =>
      if (ProverConfig.configs.isDefinedAt(c)) success
      else failure(s"Unknown prover configuration $c. Known configurations: ${ProverConfig.configs.keys.mkString(", ")}")
    } action { (c, config) =>
      config.copy(proverConfigs = config.proverConfigs :+ ProverConfig.configs(c))
    } text (s"prover configuration, one of ${ProverConfig.configs.keys.mkString(", ")}")

    opt[Unit]("fulllogs") action { (_, config) =>
      config.copy(fullLogs = true)
    } text (s"calls prover with all logging options")
    opt[Unit]("logexec") action { (_, config) =>
      config.copy(logExec = true)
    } text (s"log calls to external prover")
    opt[Unit]("logperfile") action { (_, config) =>
      config.copy(logPerFile = true)
    } text (s"log prover result per file")
    opt[Unit]("logproof") action { (_, config) =>
      config.copy(logProof = true)
    } text (s"log proofs")
    opt[Unit]("logdisproof") action { (_, config) =>
      config.copy(logDisproof = true)
    } text (s"log disproofs")
    opt[Unit]("loginconclusive") action { (_, config) =>
      config.copy(logInconclusive = true)
    } text (s"log inconclusive hints")
    opt[Unit]("logsummary") action { (_, config) =>
      config.copy(logSummary = true)
    } text (s"log summary of prover results")
    opt[File]("logcsv") action { (f, config) =>
      config.copy(logCSV = f)
    } text (s"log prover results to CSV file")
    opt[File]("logxls") action { (f, config) =>
      config.copy(logXLS = f)
    } text (s"log prover results to XLS file")
    opt[File]("logoverviewxls") action { (f, config) =>
      config.copy(logXLSOverview = f)
    } text (s"log overview of prover results to XLS file")
    opt[Int]("par") action { (f, config) =>
      config.copy(parallelism = f)
    } text (s"set parallelism level: 0 = no parallelism at all, " +
      s"1 = number of system cores - 1, greater 1 = custom parallelism level")
    opt[String]("logfile") action { (s, config) =>
      config.copy(logFilePath = s)
    } text ("Path for output of each prover")
    opt[Unit]("summarizelogs") action { (b, config) =>
      config.copy(summarizeLogs = true)
    } text ("indicates that given files are already proof logs, generates summary")
    opt[Unit]("layoutData") action { (b, config) =>
      config.copy(layoutData = true)
    } text ("indicates that argument file is excel file for producing various data layouts")
    opt[Unit]("generateSLURM") action { (b, config) =>
      config.copy(generateSLURM = true)
    } text ("argument indicates path to prover input files - generates job array SLURM script for executing the prover calls on the HHLR")
    opt[File]("sortHHLRoutput") action { (f, config) =>
      config.copy(sortHHLRoutput = f)
    } text ("argument of option is file path to HHLR output; final file argument indicates path to prover input files")

    arg[File]("<proof goal file> or <proof log file> or <excel file> ...") unbounded() validate { file =>
      if (file.exists()) success else failure(s"file not found ${file.getAbsolutePath}")
    } action { (file, config) =>
      config.copy(files = config.files :+ file.getAbsoluteFile)
    } text ("files containing proof goals or files containing proof logs (with option summarizeLogs) or excel file " +
      "for laying out data (with option layoutData) or folder with HHLR output (to be sorted with option sortHHLRoutput)")

    for (opts <- ProverConfig.contributedOptions)
      opts.contributeOptions(this)
  }

  optionParser.parse(args, Config()) match {
    case None => sys.exit(1)
    case Some(iconfig) =>
      val config = iconfig.ensureDefaultOptions

      //if logs are to be summarized, separate summaries for different timeouts (subfolders of log folders)
      if (config.summarizeLogs) {
        val folderlist = for (f <- config.files if (f.isDirectory)) yield f.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = dir.isDirectory &&
            !(name.startsWith(".")) &&
            !(name.endsWith(".tar") || name.endsWith(".bz2"))
        })


        for (dirseq <- folderlist; dir <- dirseq) {
          val newconfig1 = config.copy(files = Seq(dir))
          val dirname = dir.getName
          val time = dirname.dropRight(1).toInt
          val newconfig = newconfig1.copy(timeout = time)
          val runner = new Runner(newconfig)
          println(s"Calling runner for $dirname...")
          runner.run()

          val summary = runner.summary

          def attachDirNameToOutputFile(file: File): File = {
            val newfileName = s"$dirname-${file.getName}"
            val path = ((file.getAbsolutePath split("/")).dropRight(1)).mkString("/")
            new File(s"$path/$newfileName")
          }

          if (config.logSummary)
            print(summary.makeSummary)
          if (config.logCSV != null) {
            val newfile = attachDirNameToOutputFile(config.logCSV)
            new PrintWriter(newfile) {
              write(summary.makeCSV);
              close
            }
          }
          if (config.logXLS != null) {
            val newfile = attachDirNameToOutputFile(config.logXLS)
            summary.makeXLS.safeToFile(newfile.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
          }
          if (config.logXLSOverview != null) {
            val newfile = attachDirNameToOutputFile(config.logXLSOverview)
            summary.makeXLSOverview.safeToFile(newfile.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
          }
        }

        //mode for layouting data - directory with Excel files that are to be layouted
        //all the files in the folder should have names like this:
        // {timeout}s-[...]-[overview|raw].xls
      } else if (config.layoutData) {
        val excelfiles = for (f <- config.files) yield f.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name.endsWith(".xls")
        })

        val filenamerexp = """([0-9]+)s-[a-zA-Z]+-([a-zA-Z]+)\.xls""".r.unanchored

        val groupedfiles = for (fseq <- excelfiles) yield {
          fseq.groupBy[String](f => f.getName match {
            case filenamerexp(timeout, cl) => timeout
            case s => s.charAt(0).toString
          })
        }

        //start runner for every pair of raw/overview that you can find
        for (mseq <- groupedfiles; (t, fseq) <- mseq) {
            val rawfile = fseq.find(f => f.getName match {
              case filenamerexp(_, cl) if (cl == "raw") => true
              case _ => false
            }).getOrElse(null)
            val overviewfile =fseq.find(f => f.getName match {
              case filenamerexp(_, cl) if (cl == "overview") => true
              case _ => false
            }).getOrElse(null)

          val newconfig = config.copy(files = List(rawfile, overviewfile)).copy(timeout = t.toInt)
          val runner = new Runner(newconfig)
          runner.run()
        }
      } else {
        //normal mode (run provers), where we run runner only once
        val runner = new Runner(config)
        runner.run()

        //only execute the last steps if tool was not called for laying out data
        if (!config.layoutData) {
          val summary = runner.summary

          if (config.logSummary)
            print(summary.makeSummary)
          if (config.logCSV != null)
            new PrintWriter(config.logCSV) {
              write(summary.makeCSV);
              close
            }
          if (config.logXLS != null)
            summary.makeXLS.safeToFile(config.logXLS.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
          if (config.logXLSOverview != null)
            summary.makeXLSOverview.safeToFile(config.logXLSOverview.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
        }
      }

  }
}
