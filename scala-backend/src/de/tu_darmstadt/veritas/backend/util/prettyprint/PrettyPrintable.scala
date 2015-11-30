package de.tu_darmstadt.veritas.backend.util.prettyprint

import java.io.StringWriter

import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.Module

/**
 * Implement it when the output to the underlying writer can be used by the corresponding parser to
 * fully reconstruct the original object.
 */
trait PrettyPrintable {
  /**
   * Overwrite this in the implementations. Use [[PrettyPrintWriter.indent()]] before write()-ing
   * a substructure of this and [[PrettyPrintWriter.unindent()]] afterwards.
   */
  /* abstract */ def prettyPrint(writer: PrettyPrintWriter): Unit

  /**
   * NOTE do not use this inside a prettyPrint() body to obtain the pretty printed String
   * of a substructure, rather call substructure.prettyPrint(writer) or the convenience method
   * [[writer.write[T <: PrettyPrintable](T)]]
   * WHY? Because toPrettyString generates the String by creating a new StringWriter internally,
   * instead you could just use whatever Writer your current pretty printer is internally using!
   */
  def toPrettyString(): String = {
    val prettyPrinter = new PrettyPrintWriter(new StringWriter())
    prettyPrint(prettyPrinter)
    prettyPrinter.flush()
    val resString = prettyPrinter.toString()
    prettyPrinter.close()
    resString
  }
}

/**
 * Convenience: Mixin this when the pretty printed repr of your data is just a String
 */
trait SimplePrettyPrintable extends PrettyPrintable {
  /* abstract */ def prettyString: String
  override def prettyPrint(writer: PrettyPrintWriter): Unit = writer.write(prettyString)
}