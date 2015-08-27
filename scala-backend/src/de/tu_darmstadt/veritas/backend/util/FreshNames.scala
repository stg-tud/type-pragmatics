package de.tu_darmstadt.veritas.backend.util

class FreshNames {
  private[this] val identifierCounts = collection.mutable.Map[String, Int]().withDefaultValue(0)
  def freshName(identifier: String): String = {
    val identifierCount = identifierCounts(identifier)
    identifierCounts(identifier) += 1
    identifier + identifierCount
  }
  def freshNames(ids: Seq[String]): Seq[String] = ids map freshName
  def apply(ids: Seq[String]): Seq[String] = freshNames(ids)
}