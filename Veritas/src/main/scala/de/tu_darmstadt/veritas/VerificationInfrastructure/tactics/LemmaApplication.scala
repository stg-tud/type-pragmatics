package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}

//TODO edge information for lemma applications might have to be refined
// IDEAS: possible lemma instantiations, application hints (order...?)
case class LemmaApplication[Goal <: Ordered[Goal]](lemmaname: String) extends EdgeLabel {
  override def desc: String = lemmaname

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

  override def compare(that: EdgeLabel): Int = ???
}
