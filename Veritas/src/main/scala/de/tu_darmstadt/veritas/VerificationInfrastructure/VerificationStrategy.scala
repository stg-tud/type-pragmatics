package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Strategies for labeling edges of ProofTrees
  */
abstract class VerificationStrategy

/**
  * default strategy: simply try to figure out a proof for a node of a proof tree given its children, e.g. calling an ATP
  */
object Solve extends VerificationStrategy


object Induction extends VerificationStrategy

//TODO further strategies for verifying proof trees?
