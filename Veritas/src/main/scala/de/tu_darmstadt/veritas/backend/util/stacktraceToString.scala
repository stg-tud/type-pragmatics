package de.tu_darmstadt.veritas.backend

import java.io.PrintStream
import java.io.UnsupportedEncodingException
import java.io.ByteArrayOutputStream

package object util {
  /**
   * Throwable cannot give stack trace as string, so write the stack trace to a buffer first, 
   * then this buffer toString
   */
  def stacktraceToString(ex: Throwable) = {
    val outputStream = new ByteArrayOutputStream()
    ex.printStackTrace(new PrintStream(outputStream))
    try {
      outputStream.toString("UTF-8")
    } catch {
      case _: UnsupportedEncodingException => "Error converting stack trace to UTF8"
    }
  }
}