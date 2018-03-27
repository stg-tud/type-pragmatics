package de.tu_darmstadt.veritas.backend.util

import scala.collection.mutable.ListBuffer

object Util {
  def removeExtension(s: String): String = {
    val last = s.lastIndexOf('.')
    if (last < 0)
      return s
    return s.substring(0, last)
  }
  
  def getExtension(s: String): String = {
    val last = s.lastIndexOf('.')
    if (last < 0)
      return ""
    return s.substring(last + 1)
  }
  
  def splitExtension(s: String): (String, String) = {
    val last = s.lastIndexOf('.')
    if (last < 0)
      return (s, "")
    return (s.substring(0, last), s.substring(last + 1))
  }
  
  def generateFileName(basePath: String, ruleName: String): String = {
    val (basePrefix, ext) = splitExtension(basePath)
    val filePrefix = s"$basePrefix-$ruleName"
    s"$filePrefix.$ext"
  }

  /**
    * Found this piece of code on stackoverflow after looking for a way to do it with a scala library function
    * but there is no such function to sort based on a partial ordering
    */
  def sortByPartialOrdering[T](seq: Seq[T], lessThan: (T, T) => Boolean): Seq[T] = {
    val visited = Array.fill[Boolean](seq.length)(false)
    val postOrder = ListBuffer.empty[Int]

    def visit(n: Int): Unit = {
      visited(n) = true
      for (i <- 0 until seq.length)
        if (!visited(i) && lessThan(seq(i), seq(n)))
          visit(i)
      postOrder += n
    }

    for (i <- 0 until seq.length)
      if (!visited(i))
        visit(i)

    assert(postOrder.size == seq.length)

    postOrder map seq
  }
}