package de.tu_darmstadt.veritas.backend.util.prettyprint

trait PrettyPrintableFile extends PrettyPrintable {
  def filename: String
  def goalname: String
}

/**
 * Convenience: Ad-hoc constructor
 */
object PrettyPrintableFile {
  def apply(file: String, goal: String, content: String): PrettyPrintableFile = 
    new { val filename: String = file; val goalname = goal } with PrettyPrintableFile {
      def prettyPrint(writer: PrettyPrintWriter) = writer.write(content)
    }
}
