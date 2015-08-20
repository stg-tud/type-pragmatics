package veritas.benchmarking

import java.io.{PrintWriter, PrintStream, File}

import scopt.OptionParser

object Main extends App {

  case class Config(
    files: Seq[File] = Seq(),
    repetitions: Int = 1,
    timeout: Int = 0,
    proverConfigs: Seq[ProverConfig] = Seq(),
    logExec: Boolean = false,
    logPerFile: Boolean = false,
    logSummary: Boolean = false,
    logCSV: File = null,
    logXLS: File = null
  ) {
    def ensureDefaultOptions: Config = {
      if (!logExec && !logPerFile && !logSummary && logCSV == null && logXLS == null)
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

    opt[String]('c', "config") validate { c =>
      if (ProverConfig.configs.isDefinedAt(c)) success
      else failure(s"Unknown prover configuration $c. Known configurations: ${ProverConfig.configs.keys.mkString}")
    } action { (c, config) =>
      config.copy(proverConfigs = config.proverConfigs :+ ProverConfig.configs(c))
    } text(s"prover configuration, one of ${ProverConfig.configs.keys.mkString(", ")}")

    opt[Unit]("logexec") action { (_, config) =>
      config.copy(logExec = true)
    } text(s"log calls to external prover")
    opt[Unit]("logperfile") action { (_, config) =>
      config.copy(logPerFile = true)
    } text(s"log prover result per file")
    opt[Unit]("logsummary") action { (_, config) =>
      config.copy(logSummary = true)
    } text(s"log summary of prover results")
    opt[File]("logcsv") action { (f, config) =>
      config.copy(logCSV = f)
    } text(s"log prover results to CSV file")
    opt[File]("logxls") action { (f, config) =>
      config.copy(logXLS = f)
    } text(s"log prover results to XLS file")

    arg[File]("<proof goal file>...") unbounded() validate { file =>
      if (file.exists()) success else failure(s"file not found ${file.getAbsolutePath}")
    } action { (file, config) =>
      config.copy(files = config.files :+ file.getAbsoluteFile)
    } text("files containing proof goals")
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
        new PrintWriter(config.logCSV) { write(summary.makeCSV); close }
      if (config.logXLS != null)
        summary.makeXLS.safeToFile(config.logXLS.getAbsolutePath).fold(ex ⇒ throw ex, identity).unsafePerformIO
  }
}
