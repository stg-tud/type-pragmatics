package veritas.benchmarking

trait ProverConfig {

}

object ProverConfig {
  private var _configs: Map[String, ProverConfig] = Map()
  _configs += "default" -> DefaultProverConfig

  def configs = _configs
}

object DefaultProverConfig extends ProverConfig