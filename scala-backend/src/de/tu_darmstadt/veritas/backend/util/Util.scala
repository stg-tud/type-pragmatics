package de.tu_darmstadt.veritas.backend.util

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
}