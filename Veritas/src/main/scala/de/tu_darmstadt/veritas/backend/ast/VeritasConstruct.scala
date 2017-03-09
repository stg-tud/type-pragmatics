package de.tu_darmstadt.veritas.backend.ast

/**
 * superclass of all Veritas constructs
 * used for defining general operations on Veritas constructs
 */
trait VeritasConstruct extends Ordered[VeritasConstruct] {
  /**
   * has to be overridden by each VeritasConstruct!
   * declares the children each Veritas construct has,
   * the grouping of children, the order of the groups 
   * and the order of the children within the groups
   */
  val children: Seq[Seq[VeritasConstruct]]

  override def compare(that: VeritasConstruct): Int = {
    val hcompare = this.hashCode compare that.hashCode
    if (hcompare != 0)
      return hcompare
    if (this == that)
      return 0
    throw new RuntimeException(s"Failed to compare $this and $that using hash codes.")
  }
}