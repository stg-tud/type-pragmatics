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

final class Impl private(val argLeft: FofUnitary, val argRight: FofUnitary) 
  extends FofBinaryNonassoc(argLeft, argRight, " => ") {
  override def toString = s"Impl($argLeft, $argRight)"
}
object Impl {
  def apply(argLeft: FofUnitary, argRight: FofUnitary) = new Impl(argLeft, argRight)
//    (argLeft, argRight) match {
//    case (True, _) => argRight
//    case (False, _) => True
//    case (_, True) => True
//    case (_, False) => Not(argLeft)
//    case (p1, p2) if p1 == p2 => True
//    case _ => new Impl(argLeft, argRight)
//  }
  
  def unapply(e: Impl): Option[(FofUnitary, FofUnitary)] = Some((e.argLeft, e.argRight))
}

final case class BiImpl(argLeft: FofUnitary, argRight: FofUnitary)
  extends FofBinaryNonassoc(argLeft, argRight, " <=> ")

// TODO do we need fof_sequent?
// FofLogicFormula (non sequent) vs FofSequent (sequent), where both extends FofFormula

