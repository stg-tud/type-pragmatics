package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintableFile

final case class FofFile(filename: String, content: Seq[FofAnnotated]) extends PrettyPrintableFile {
  override def prettyPrint(writer: PrettyPrintWriter) = content foreach (writer.writeln(_))
}
