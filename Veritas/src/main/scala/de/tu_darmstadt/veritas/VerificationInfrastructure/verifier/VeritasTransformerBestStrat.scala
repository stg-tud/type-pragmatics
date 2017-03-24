package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct
import de.tu_darmstadt.veritas.backend.fof.FofFile
import de.tu_darmstadt.veritas.backend.tff.TffFile

trait TPTP extends VerifierFormat

case class FOFFormat(fof: FofFile) extends TPTP

case class TFFFormat(tff: TffFile) extends TPTP

/**
  * Translate VeriTaS module to TFF using the "most advantegeous strategies we determined so far
  * (module transformations)
  */
class VeritasTransformerBestStrat extends Transformer[Seq[VeritasConstruct], VeritasConstruct, TFFFormat]{
  override def transformProblem(spec: Seq[VeritasConstruct], goal: VeritasConstruct): TFFFormat = ???
}
