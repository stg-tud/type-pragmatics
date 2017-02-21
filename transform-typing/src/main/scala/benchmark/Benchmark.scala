package benchmark

import java.io.{File, FileWriter}

import system._
import system.Verification.{ProofObligation, ProverResult}
import system.optimize.{DropUnreachableDefinitions, GoalNormalization, GoalUnpacking, RuleStrengthening}
import veritas.benchmarking.Proved

import scala.collection.immutable.ListMap

object Benchmark extends App {

  object OptimizationConfig extends Enumeration {
    type OptimizationConfig = Value
    val All, NoUnpack, NoNormalization, NoStrengthening, NoDropUnreachable = Value

    def optimize(config: OptimizationConfig, obl: ProofObligation): Seq[ProofObligation] = config match {
      case All =>
        val obls1 = GoalUnpacking.unpackObligation(obl)
        val obls2 = obls1.flatMap(GoalNormalization.normalizeObligation(_))
        val obls3 = obls2.map(RuleStrengthening.strengthenObligation(_))
        val obls4 = obls3.map(DropUnreachableDefinitions.dropUnreachable(_))
        obls4
      case NoUnpack =>
        val obls1 = Seq(obl)
        val obls2 = obls1.flatMap(GoalNormalization.normalizeObligation(_))
        val obls3 = obls2.map(RuleStrengthening.strengthenObligation(_))
        val obls4 = obls3.map(DropUnreachableDefinitions.dropUnreachable(_))
        obls4
      case NoNormalization =>
        val obls1 = GoalUnpacking.unpackObligation(obl)
        val obls2 = obls1
        val obls3 = obls2.map(RuleStrengthening.strengthenObligation(_))
        val obls4 = obls3.map(DropUnreachableDefinitions.dropUnreachable(_))
        obls4
      case NoStrengthening =>
        val obls1 = GoalUnpacking.unpackObligation(obl)
        val obls2 = obls1.flatMap(GoalNormalization.normalizeObligation(_))
        val obls3 = obls2
        val obls4 = obls3.map(DropUnreachableDefinitions.dropUnreachable(_))
        obls4
      case NoDropUnreachable =>
        val obls1 = GoalUnpacking.unpackObligation(obl)
        val obls2 = obls1.flatMap(GoalNormalization.normalizeObligation(_))
        val obls3 = obls2.map(RuleStrengthening.strengthenObligation(_))
        val obls4 = obls3
        obls4
    }
  }
  import OptimizationConfig._

  case class VerificationSummary(results: ListMap[ProofObligation, ProverResult]) {
    def successCount = results.filter(_._2.status == Proved).size
    def failedCount = results.filter(_._2.status != Proved).size
    def successTimeSum = results.filter(_._2.status == Proved).map(_._2.timeSeconds.get).sum

    def writeSumamryCSV(implicit writer: FileWriter): Unit = {
      write(results.size)
      write(successCount)
      write(successTimeSum)
    }

    def writeDetailsCSV(trans: Transformation, config: OptimizationConfig)(implicit writer: FileWriter): Unit ={
      for ((obl, res) <- results) {
        write(trans.contractedSym)
        write(config)
        write(obl.name)
        write(res.status)
        write(res.timeSeconds.getOrElse(-1.0))
        writer.append('\n')
      }
    }
  }
  case class Result(
    trans: Transformation,
    config: OptimizationConfig,
    completeness: VerificationSummary,
    contractCompliance: VerificationSummary,
    soundness: VerificationSummary) {

    def writeCSV(implicit writer: FileWriter, detailedWriter: FileWriter) {
      write(trans.contractedSym)(writer)
      write(config)(writer)
      completeness.writeSumamryCSV(writer)
      contractCompliance.writeSumamryCSV(writer)
      soundness.writeSumamryCSV(writer)

      completeness.writeDetailsCSV(trans, config)(detailedWriter)
      contractCompliance.writeDetailsCSV(trans, config)(detailedWriter)
      soundness.writeDetailsCSV(trans, config)(detailedWriter)
    }

  }

  def runCompleteness(trans: Transformation, config: OptimizationConfig): VerificationSummary = {
    val completeness = trans.forall(t => Completeness.completenessTrans(t)).values.flatten
    val obls = completeness.flatMap(optimize(config, _))
    val timeout = trans.completenessTimeout
    val mode = trans.completenessMode
    val results = ListMap() ++ obls.map(obl => obl -> Verification.verify(obl, mode, timeout))
    VerificationSummary(results)
  }

  def runContractCompliance(trans: Transformation, config: OptimizationConfig): VerificationSummary = {
    val contractCompliance = trans.forall(t => ContractCompliance.complianceTrans(t)).values.flatten
    val obls = contractCompliance.flatMap(optimize(config, _))
    val timeout = trans.contractComplianceTimeout
    val mode = trans.contractComplianceMode
    val results = ListMap() ++ obls.map(obl => obl -> Verification.verify(obl, mode, timeout))
    VerificationSummary(results)
  }

  def runSoundness(trans: Transformation, config: OptimizationConfig): VerificationSummary = {
    val soundness = trans.forall(t => Soundness.soundnessTrans(t)).values.flatten
    val obls = soundness.flatMap(optimize(config, _))
    val timeout = trans.soundnessTimeout
    val mode = trans.soundnessMode
    val results = ListMap() ++ obls.map(obl => obl -> Verification.verify(obl, mode, timeout))
    VerificationSummary(results)
  }

  def run(trans: Transformation, config: OptimizationConfig): Result = {
    val completeness = runCompleteness(trans, config)
    val contractCompliance = runContractCompliance(trans, config)
    val soundness = runSoundness(trans, config)
    Result(trans, config, completeness, contractCompliance, soundness)
  }


  def write(s: Any)(implicit writer: FileWriter) = writer.append('"').append(s.toString).append('"').append('\t')

  val csv = new File(args(0))
  val detailedCsv = new File(args(1))
  csv.delete()
  detailedCsv.delete()

  val transformations = {
    if (args.size >= 3) args(2) match {
      case "let" => Seq(let.let_desugar)
      case "delta" => Seq(delta.edelta)
      case "cps" => Seq(cps.ecps)
    }
    else Seq(let.let_desugar, delta.edelta, cps.ecps)
  }

  for (trans <- transformations;
       config <- OptimizationConfig.values) {
    val result = run(trans, config)
    val writer = new FileWriter(csv, true)
    val detailedWriter = new FileWriter(detailedCsv, true)
    result.writeCSV(writer, detailedWriter)
    writer.append('\n')
    writer.close()
    detailedWriter.close()
  }
}
