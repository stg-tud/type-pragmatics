package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer, PropagatableInfo}

//TODO edge information for lemma applications could be refined
// IDEAS for later: possible lemma instantiations, application hints (order...?)
case class LemmaApplicationStep[Goal <: Ordered[Goal]](lemmaname: String) extends EdgeLabel {
  override def desc: String = lemmaname

  // current decision: lemma applications should not forward any propagatable info (such as fixed variables
  // or induction hypotheses) since the proof of a lemma has to be possible independently of where in the
  // proof graph it appears
  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

  override def compare(that: EdgeLabel): Int = that match {
    case lem: LemmaApplicationStep[Goal] => this.lemmaname compare lem.lemmaname
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

case class LemmaApplication[Defs <: Ordered[Defs], Formulae <: Defs with Ordered[Formulae]](lemmas: Seq[Formulae], spec: Defs, queryspec: SpecEnquirer[Defs, Formulae]) extends Tactic[Defs, Formulae] {

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
      val lemmaobl = produce.newObligation(spec, lemma)
      val lemmaedge = LemmaApplicationStep(getFormulaName(lemma))
      (lemmaobl, lemmaedge)
    }
  }

  override def compare(that: Tactic[Defs, Formulae]): Int = that match {
    case lem: LemmaApplication[Defs, Formulae] => {
      val lemmacomp = this.lemmas.length compare lem.lemmas.length
      if (lemmacomp == 0) {
        lazy val individual_comp = this.lemmas zip lem.lemmas map { case (l1, l2) => l1 compare l2 }
        (individual_comp find (_ != 0)).getOrElse(this.spec compare lem.spec)
      }
      else
        lemmacomp
    }
  }
}
