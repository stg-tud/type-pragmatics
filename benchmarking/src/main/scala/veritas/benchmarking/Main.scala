package veritas.benchmarking

import java.io.File

import scopt.OptionParser

object Main extends App {

  case class Config(
    files: Seq[File] = Seq(),
    repetitions: Int = 1,
    timeout: Int = 0,
    proverConfig: ProverConfig = ProverConfig.configs.head._2
  )

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
      config.copy(proverConfig = ProverConfig.configs(c))
    } text(s"prover configuration, one of ${ProverConfig.configs.keys.mkString(", ")}")

    arg[File]("<proof goal file>...") validate { file =>
      if (file.exists()) success else failure(s"file not found ${file.getAbsolutePath}")
    } action { (file, config) =>
      config.copy(files = config.files :+ file.getAbsoluteFile)
    } text("files containing proof goals")
  }

  optionParser.parse(args, Config()) match {
    case None => sys.exit(1)
    case Some(config) => new Runner().run(config)
  }
}
