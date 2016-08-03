package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.veritas.Module

object Fixpoint {
  val MAX = 5
}

case class Fixpoint(trans: ModuleTransformation) extends ModuleTransformation {
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
	  var it = 0
	  def run(m: Seq[Module]): Seq[Module] = {
		  if (it >= Fixpoint.MAX)
			  m
		  else {
		    it += 1
  		  val result = trans.apply(m)
  		  if (result != m)
  			  run(result)
  		  else
  			  result
		  }
	  }
	  run(m)
  }
}