package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Strategies for labeling edges of ProofTrees
  */
abstract class VerificationStrategy

/**
  * default strategy: simply try to figure out a proof for a node of a proof tree given its children, e.g. calling an ATP
  */

case object Solve extends VerificationStrategy

//TODO maybe refine this later
case object Induction extends VerificationStrategy

//TODO which other abstract strategies are there for verifying proof trees?
