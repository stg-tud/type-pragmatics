package de.tu_darmstadt.veritas.backend

object Configuration {
  
  trait ConfigOption extends Enumeration 
  type ConfigParameter = ConfigOption
  type ConfigValue = Any
  
  object LogicalSimplification extends ConfigOption {
    val Off, On = Value
  }
  
  object VariableEncoding extends ConfigOption {
    val Unchanged, NameEverything, InlineEverything, NameParamsAndResults = Value
  }
  
  object InversionLemma extends ConfigOption {
    val Off, On = Value
  }
  
  object FinalEncoding extends ConfigOption {
    val BareFOF, GuardedFOF, TFF = Value
  }
  
  object Problem extends ConfigOption {
    val Consistency, Proof, Test = Value
  }
  
  def ifConfig(p: ConfigParameter, v: ConfigValue) = (cfg: Configuration) => cfg.m.get(p).map(_==v).getOrElse(false)
  def selectConfig[T](p: ConfigParameter)(select: ConfigValue => T) = (cfg: Configuration) => select(cfg.m(p))
}

import Configuration._
case class Configuration(m: Map[ConfigParameter, ConfigValue]) extends VariabilityModel {
  def contains(other: Configuration) = this == other
  override def iterator = Iterator(this)
}

trait VariabilityModel extends Iterable[Configuration] {
  def contains(config: Configuration): Boolean
}


