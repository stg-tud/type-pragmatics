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
    val Off = Value("nosimpl")
    val On = Value("dosimpl")
  }

  object VariableEncoding extends ConfigOption {
    val Unchanged = Value("unchanged")
    val NameEverything = Value("nameevery")
    val InlineEverything = Value("inlievery")
    val NameParamsAndResults = Value("namparres")
  }

  object TypingJudgmentEncoding extends ConfigOption {
    val Function, Predicate = Value
  }

  // this variable is currently removed, we decided on not having this variation in the study
  //  object InversionLemma extends ConfigOption {
  //    val Off = Value("noinv") 
  //    val On  = Value("doinv")
  //  }

  object FinalEncoding extends ConfigOption {
    val BareFOF, GuardedFOF, TFF = Value
  }

  object Problem extends ConfigOption {
    val Consistency, Proof, Test, Execution, Synthesis, Counterexample, All = Value
  }

  def ifConfig(p: ConfigParameter, v: ConfigValue) = (cfg: Configuration) => cfg.m.get(p).map(_ == v).getOrElse(false)
  def selectConfig[T](p: ConfigParameter)(select: ConfigValue => T) = (cfg: Configuration) => select(cfg.m(p))
}
case class Configuration(m: Map[ConfigParameter, ConfigValue]) extends VariabilityModel {
  def apply(p: ConfigParameter) = m(p)
  def contains(other: Configuration) = this == other
  override def iterator = Iterator(this)
  override def toString = s"Configuration($m)"
}

trait VariabilityModel extends Iterable[Configuration]

object FullVariability extends VariabilityModel {
  override def iterator = for (
    typ <- TypingJudgmentEncoding.iterator;
    simpl <- LogicalSimplification.iterator;
    vars <- VariableEncoding.iterator;
    fin <- FinalEncoding.iterator;
    prob <- Problem.iterator if prob != Problem.All
  ) yield Configuration(Map(
    TypingJudgmentEncoding -> typ,
    LogicalSimplification -> simpl,
    VariableEncoding -> vars,
    FinalEncoding -> fin,
    Problem -> prob))
}

case class PartialVariability(config: Map[ConfigParameter, Seq[ConfigValue]]) extends VariabilityModel {
  private def test(p: ConfigParameter, v: ConfigValue) = config.get(p).map(_.contains(v)).getOrElse(true)

  override def iterator = for (
    typ <- TypingJudgmentEncoding.iterator if test(TypingJudgmentEncoding, typ);
    simpl <- LogicalSimplification.iterator if test(LogicalSimplification, simpl);
    vars <- VariableEncoding.iterator if test(VariableEncoding, vars);
    fin <- FinalEncoding.iterator if test(FinalEncoding, fin);
    prob <- Problem.iterator if test(Problem, prob)
  ) yield Configuration(Map(
    TypingJudgmentEncoding -> typ,
    LogicalSimplification -> simpl,
    VariableEncoding -> vars,
    FinalEncoding -> fin,
    Problem -> prob))
}

