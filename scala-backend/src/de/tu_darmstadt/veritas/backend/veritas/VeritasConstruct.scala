package de.tu_darmstadt.veritas.backend.veritas

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
  
  /**
   * has to be overridden by each VeritasConstruct!
   * produces a new copy of the construct, replacing all the children of the construct
   * with the new children given in the parameter
   * 
   * 
   * @throws ClassCastException if the children do not have the expected type!
   */
  def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct
  
}