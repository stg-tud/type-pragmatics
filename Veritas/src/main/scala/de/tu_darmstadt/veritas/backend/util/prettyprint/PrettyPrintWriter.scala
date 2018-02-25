package de.tu_darmstadt.veritas.backend.util.prettyprint

import java.io.StringWriter
import java.io.Writer

/**
 * Decorated Writer for pretty printing
 * @param defaultIndentation the string to append to the next indentations when calling indent()
 * @param linebreak writeln() uses this, give it "" to pretty print without additional newlines
 */
class PrettyPrintWriter(writer: Writer = new StringWriter,
                        linewidth: Int = 100,
                        defaultIndentation: String = "  ",
                        linebreak: String = "\n") extends Writer {

  private val indentation = new IndentationStack
  private val currentLineBuffer = new LineBuffer(linewidth)

  /**
   * Call this before a pretty printing new "substructure" of your data
   * NOTE prints a newline!
   */
  def indent(indentChars: String = defaultIndentation): this.type = {
    write(linebreak)
    indentation.push(ForcedIndent(indentChars))
    this
  }

  def indentOptional(indentChars: String = defaultIndentation): this.type = {
    // two adjacent optional indents should become two forced indents
    if (indentation.isCurrentOptional) {
      indent(indentChars)
    } else {
      // flush the current line contents, since we will insert the (potential) linebreak after this
      this.flush()
      indentation.push(OptionalIndent(indentChars))
      this
    }
  }

  def newline(): this.type = {
    write(linebreak)
    this
  }

  /**
   * Removes the last added indentation
   */
  def unindent(): this.type = {
    // flush, so that the current line's indent is not affected by the pop
    this.flush()
    indentation.pop()
    this
  }

  /*
   * Writer interface
   */

  override def write(str: Array[Char], off: Int, len: Int): Unit = {
    // to conform with the Writer signature
    if (off < 0 || len < 0 || off + len < 0 || off + len > str.length)
      throw new IndexOutOfBoundsException
    val subString = String.valueOf(str.slice(off, off + len))

    // NOTE match against \r\n to work also for CRLF line endings
    val lines = subString.split("\r?\n", /* IMPORTANT for trailing empty lines! */ -1)
    val (appendToCurrentLine, newLines) = (lines.head, lines.tail)
    writeLines(lines.head, lines.tail)
  }

  override def close(): Unit = {
    this.flush(); writer.close()
  }

  /**
   * Write the pending line first, then flush the underlying writer
   * NOTE: Only writes the indentation of the current line, if it contains any actual content,
   * because for empty lines unindent() will change the current indentation
   */
  private var linePartiallyWritten = false
  override def flush(): Unit = {
    // do not write the indentation if the buffer currently is empty anyways -> no indentation
    // for empty lines
    // do not write the pending line, if it is indentOptional -> might need to insert a linebreak
    // later on
    // NOTE this means, we cannot every toPrettyString a sole indentOptional() without unindent()!
    if (!linePartiallyWritten && !currentLineBuffer.isEmpty && !indentation.isCurrentOptional) {
      writer.write(indentation.toString)
      writer.write(currentLineBuffer.readAndRemoveContents())
      linePartiallyWritten = true
    }
    writer.flush()
  }

  /**
   * Main method: handles
   *  - partially written lines by explicit flush()-ing
   *  - indentOptional
   *  - trimming whitespace at line endings
   */
  private def writeLines(appendToCurrentLine: String, newLines: Seq[String]): Unit = {
    currentLineBuffer ++= appendToCurrentLine

    if (indentation.isCurrentOptional) {
      if (currentLineBuffer.length + indentation.length > linewidth || !newLines.isEmpty) {
        writer.write(linebreak)
        indentation.setCurrentForced()
        currentLineBuffer.newLine()
        linePartiallyWritten = false
      }
    }

    if (!newLines.isEmpty) {
      // write the pending line
      var lineToPrint = ""
      if (!linePartiallyWritten) {
        lineToPrint += indentation
      }
      lineToPrint += currentLineBuffer.readAndRemoveContents()
      // trim trailing whitespace
      writer.write(lineToPrint.replaceAll("\\s+$", "") + linebreak)
      currentLineBuffer.newLine()
      linePartiallyWritten = false

      writeLines(newLines.head, newLines.tail)
    }
  }

  /*
   * Convenience methods below:
   */

  override def toString() = { this.flush(); writer.toString }

  /**
   * Convenience for many strings.
   * The calls are also chainable, since it returns this again
   */
  // NOTE "def write(String*): this.type" would be ambiguous with the Writer interface write, so:
  def write(str1: String, str2: String, strs: String*): this.type = {
    write(str1); write(str2); strs foreach (write(_))
    this
  }

  def writeln(strs: String*): this.type = {
    strs foreach (write(_))
    write(linebreak)
    this
  }

  /**
   * Convenience for recursively printing substructures
   */
  def write[T <: PrettyPrintable](inputs: T*): this.type = {
    inputs foreach (_.prettyPrint(this))
    this
  }
  
  // NOTE have to add explicit input1 to disambiguate from writeln(String*) for empty varargs given
  def writeln[T <: PrettyPrintable](input1: T, inputs: T*): this.type = {
    write(input1).write(inputs: _*)
    write(linebreak)
    this
  }
}
