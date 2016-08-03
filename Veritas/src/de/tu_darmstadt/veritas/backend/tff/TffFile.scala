package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintableFile

final case class TffFile(filename: String, goalname: String, content: Seq[TffAnnotated]) extends PrettyPrintableFile {
  override def prettyPrint(writer: PrettyPrintWriter) = content foreach (writer.writeln(_))
}
