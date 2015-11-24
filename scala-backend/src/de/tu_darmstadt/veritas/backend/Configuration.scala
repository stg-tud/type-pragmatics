package de.tu_darmstadt.veritas.backend

import Configuration._
import scala.util.matching.Regex
import scala.reflect.NameTransformer

object Configuration {

  type ConfigParameter = ConfigOption
  type ConfigValue = Any

  trait ConfigOption extends Enumeration with Iterable[ConfigValue] {
    override def iterator = values.iterator
    override def toString =
      ((getClass.getName stripSuffix NameTransformer.MODULE_SUFFIX_STRING split '.').last split
         Regex.quote(NameTransformer.NAME_JOIN_STRING)).last
  }
  
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
case class Configuration(m: Map[ConfigParameter, ConfigValue]) extends VariabilityModel {
  def apply(p: ConfigParameter) = m(p)
  def contains(other: Configuration) = this == other
  override def iterator = Iterator(this)
  override def toString = s"Configuration($m)"
}

trait VariabilityModel extends Iterable[Configuration] {
  def contains(config: Configuration): Boolean
}

object FullVariability extends VariabilityModel {
  override def contains(config: Configuration): Boolean = true
  override def iterator = for(
      simpl <- LogicalSimplification.iterator;
      vars <- VariableEncoding.iterator;
      inv <- InversionLemma.iterator;
      fin <- FinalEncoding.iterator;
      prob <- Problem.iterator
    ) yield Configuration(Map(
        LogicalSimplification -> simpl,
        VariableEncoding -> vars,
        InversionLemma -> inv,
        FinalEncoding -> fin,
        Problem -> prob
      ))
}

