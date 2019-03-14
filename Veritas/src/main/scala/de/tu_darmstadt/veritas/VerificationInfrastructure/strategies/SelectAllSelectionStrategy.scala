package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

case class SelectAllSelectionStrategy[
Type,
FDef,
Prop,
Equation,
Criteria,
Expression]()
  extends LemmaSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression] {
  override def selectLemma(dsk: DomainSpecificKnowledge[Type, FDef, Prop], acg: AugmentedCallGraph[Equation, Criteria, Expression], fn: String): Seq[Prop] = {
    val progresslemmas = dsk.lookupByFunName(dsk.progressProperties, fn)
    val preservationlemmas = dsk.lookupByFunName(dsk.preservationProperties, fn)
    val otherlemmas = dsk.lookupByFunName(dsk.auxiliaryProperties, fn)

    (progresslemmas ++ preservationlemmas ++ otherlemmas).toSeq

  }
}

