package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import scala.sys.process.{stringToProcess, ProcessLogger}
import scala.util.Random

trait Evidence extends Ordered[Evidence]

// TODO how should evidence be ordered?
trait TSTP[T] extends Evidence {
  def getData: T
}

object Evidence {
  type EvidenceChecker[Ev <: Evidence] = Ev => Boolean
  type AnyEvidenceChecker = EvidenceChecker[Evidence]

  val failing: AnyEvidenceChecker = _ => false
  val trusting: AnyEvidenceChecker = _ => true

  trait TSTPChecker extends EvidenceChecker[TSTP[File]] {
    def jarPath: File

    def apply(evidence: TSTP[File]): Boolean = {
      var outputLines = Seq[String]()
      var errorLines = Seq[String]()
      val lineLogger = ProcessLogger(line => outputLines = line +: outputLines, line => errorLines = line +: errorLines)
      val filePath = evidence.getData.getAbsolutePath
      s"java -jar $jarPath -s GDV---0.0 -q0 $filePath" ! lineLogger

      if (errorLines.nonEmpty) {
        false
      } else {
        val statusPrefix = "SZS status "
        var isVerified = false
        for (line <- outputLines) {
          if (line.startsWith(statusPrefix)) {
            val status = line.substring(statusPrefix.length)
            if (status == "Verified")
              isVerified = true
            else if (status == "NotVerified")
              isVerified = false
          }
        }
        isVerified
      }
    }
  }

  def sampling[Ev <: Evidence](rate: Double, checker: EvidenceChecker[Ev]): EvidenceChecker[Ev] = (ev: Ev) =>
    if (Random.nextDouble() < rate)
      checker(ev)
    else
      true
}
