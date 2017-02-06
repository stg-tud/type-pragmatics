package de.tu_darmstadt.veritas.backend.ast

/**
 * superclass of all Veritas constructs
 * used for defining general operations on Veritas constructs
 */
trait VeritasConstruct {
  /**
   * has to be overridden by each VeritasConstruct!
   * declares the children each Veritas construct has,
   * the grouping of children, the order of the groups 
   * and the order of the children within the groups
   */
  val children: Seq[Seq[VeritasConstruct]]
}