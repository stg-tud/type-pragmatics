package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer, PropagatableInfo}
import de.tu_darmstadt.veritas.backend.ast.Goals

//TODO edge information for lemma applications could be refined
// IDEAS for later: possible lemma instantiations, application hints (order...?)
case class LemmaApplicationStep[Goal](lemmaname: String) extends EdgeLabel {
  override def desc: String = lemmaname

  // current decision: lemma applications should not forward any propagatable info (such as fixed variables
  // or induction hypotheses) since the proof of a lemma has to be possible independently of where in the
  // proof graph it appears
  override def propagateInfoList: Seq[PropagatableInfo] = Seq()
}

case class LemmaApplication[Defs, Formulae <: Defs](lemmas: Seq[Formulae], queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

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
    for (lemma <- lemmas) yield {
      val lemmaobl = produce.newObligation(queryspec.fullspec, makeNamedGoal(lemma, getFormulaName(lemma)))
      val lemmaedge = LemmaApplicationStep(getFormulaName(lemma))
      (lemmaobl, lemmaedge)
    }
  }

  def selectLemma[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
    required.find(_._2.asInstanceOf[LemmaApplicationStep[Formulae]].lemmaname == name) match {
      case Some((obl, edge)) => obl
      case None => sys.error(s"Lemma to be selected not found: $name")
    }
}
