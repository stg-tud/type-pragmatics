package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer, PropagatableInfo}

case class ManualSimplificationStep[Goal](name: String, pinf: Seq[PropagatableInfo]) extends EdgeLabel {
  override def desc: String = name

  // has to always forward propagatable info!
  override def propagateInfoList: Seq[PropagatableInfo] = pinf
}


class ManualSimplification[Defs, Formulae <: Defs](simplifiedStep: Formulae, spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {
  import queryspec._

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  override def apply[Obligation](obl: GenObligation[Defs, Formulae],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Defs, Formulae, Obligation]): Iterable[(Obligation, EdgeLabel)] = {

      val simpobl = produce.newObligation(spec, simplifiedStep)
      val edge = ManualSimplificationStep(getFormulaName(simplifiedStep), obtainPropagatableInfo(obllabels))
      Seq((simpobl, edge))
  }

  def selectStep[Obligation](required: Iterable[(Obligation, EdgeLabel)]): Obligation =
    required.head._1 //always simply return the first sub-obligation (currently assuming that there always is only one!)

}
