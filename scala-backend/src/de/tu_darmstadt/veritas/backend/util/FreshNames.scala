package de.tu_darmstadt.veritas.backend.util

/**
 * Usage: create one instance per "namespace", i.e. where generated names should not clash.
 *
 * @param appendZero should the first instance of a name get a "0" appended or just be the name.
 *                   (The next instance of the name will get "1" appended regardless.)
 *                   I.e. with appendZero = true we get freshNames("bla", "bla") = ["bla0", "bla1"],
 *                   with appendZero = false it will be freshNames("bla", "bla") = ["bla", "bla1"].
 */
class FreshNames(appendZero: Boolean = true) {

  private[this] val identifierCounts = collection.mutable.Map[String, Int]().withDefaultValue(0)

  def freshName(identifier: String): String = {
    val identifierCount = identifierCounts(identifier)
    identifierCounts(identifier) += 1
    val newid = identifier + (if (identifierCount != 0 || appendZero) "-" + identifierCount else "")
    if (newid != identifier && identifierCounts(newid) > 0)
      freshName(newid)
    else
      newid
  }

  def freshNames(ids: Seq[String]): Seq[String] = ids map freshName
  def apply(ids: Seq[String]): Seq[String] = freshNames(ids)

}