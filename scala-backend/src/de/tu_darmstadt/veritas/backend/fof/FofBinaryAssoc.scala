package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

sealed abstract class FofBinaryAssoc(args: Seq[FofUnitary], opString: String) extends Fof {
  require(args.size >= 2, "binary op with less than 2 arguments is useless")
  
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(args.head)
    args.tail foreach {
      writer.write(opString)
      writer.write(_)
    }
  }
}

final case class Or(args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " | ")
final case class And(args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " & ")
