package veritas.benchmarking

import java.io.{PrintWriter, PrintStream, File}

import scopt.OptionParser

object Main extends App {

  case class Config(
                     files: Seq[File] = Seq(),
                     repetitions: Int = 1,
                     timeout: Int = 0,
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
                     logXLSOverview: File = null
                     ) {
    def ensureDefaultOptions: Config = {
      if (!logExec && !logPerFile && !logProof && !logDisproof && !logInconclusive && !logSummary && logCSV == null && logXLS == null)
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
      else failure(s"Unknown prover configuration $c. Known configurations: ${ProverConfig.configs.keys.mkString}")
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

    arg[File]("<proof goal file>...") unbounded() validate { file =>
      if (file.exists()) success else failure(s"file not found ${file.getAbsolutePath}")
    } action { (file, config) =>
      config.copy(files = config.files :+ file.getAbsoluteFile)
    } text ("files containing proof goals")

    for (opts <- ProverConfig.contributedOptions)
      opts.contributeOptions(this)
  }

  optionParser.parse(args, Config()) match {
    case None => sys.exit(1)
    case Some(iconfig) =>
      val config = iconfig.ensureDefaultOptions
      val runner = new Runner(config)
      runner.run()
      val summary = runner.summary

      if (config.logSummary)
        print(summary.makeSummary)
      if (config.logCSV != null)
        new PrintWriter(config.logCSV) {
          write(summary.makeCSV); close
        }
      if (config.logXLS != null)
        summary.makeXLS.safeToFile(config.logXLS.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
      if (config.logXLSOverview != null)
        summary.makeXLSOverview.safeToFile(config.logXLSOverview.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
  }
}
