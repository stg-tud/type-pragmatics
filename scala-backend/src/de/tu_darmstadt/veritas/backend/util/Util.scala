package de.tu_darmstadt.veritas.backend.util

object Util {
  def removeExtension(s: String): String = {
    val last = s.lastIndexOf('.')
    if (last < 0)
      return s
    return s.substring(0, last)
  }
}