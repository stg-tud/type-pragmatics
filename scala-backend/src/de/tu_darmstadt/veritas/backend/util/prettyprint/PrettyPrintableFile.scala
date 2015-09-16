package de.tu_darmstadt.veritas.backend.util.prettyprint

trait PrettyPrintableFile extends PrettyPrintable {
  /* abstract */ def filename: String
}

/**
 * Convenience: Ad-hoc constructor
 */
object PrettyPrintableFile {
  def apply(file: String, content: String): PrettyPrintableFile = 
    new { val filename: String = file } with PrettyPrintableFile {
      def prettyPrint(writer: PrettyPrintWriter) = writer.write(content)
    }
}
