package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

sealed abstract class FofBinaryNonassoc(argLeft:  FofUnitary, 
                                        argRight: FofUnitary, 
                                        opString: String) extends Fof {
    override def prettyPrint(writer: PrettyPrintWriter) = {
      writer.write(argLeft).write(opString)
      writer.write(argRight)
    }
}

final case class Impl(argLeft: FofUnitary, argRight: FofUnitary) 
  extends FofBinaryNonassoc(argLeft, argRight, " => ")
final case class BiImpl(argLeft: FofUnitary, argRight: FofUnitary)
  extends FofBinaryNonassoc(argLeft, argRight, " <=> ")

// TODO do we need fof_sequent?
// FofLogicFormula (non sequent) vs FofSequent (sequent), where both extends FofFormula

