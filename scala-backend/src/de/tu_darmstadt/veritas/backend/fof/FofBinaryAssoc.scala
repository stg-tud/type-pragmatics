package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

sealed abstract class FofBinaryAssoc(args: Seq[FofUnitary], opString: String) extends Fof {
  require(args.size >= 2, "And/Or should never be created with less than 2 arguments, see the companion objects")
  
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(args.head)
    args.tail foreach {
      writer.write(opString)
      writer.write(_)
    }
  }
}


final class Or private(val args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " | ") {
  override def toString = args.mkString("Or(", ", ", ")")
}
object Or {
  def apply(args: Seq[FofUnitary]): Fof = args match {
    case Seq() => False
    case Seq(single) => single
    case elems if elems.contains(True) => True
    case elems => Or(elems.filterNot { 
      case False => true
      case _ => false
    })
  }
  def unapply(e: Or): Option[Seq[FofUnitary]] = Some(e.args)
}


final class And private (val args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " & ") {
  override def toString = args.mkString("And(", ", ", ")")
}
object And {
  def apply(args: Seq[FofUnitary]): Fof = args match {
    case Seq() => True
    case Seq(single) => single
    case elems if elems.contains(False) => False
    case elems => And(elems.filterNot { 
      case True => true
      case _ => false
    })
  }
  def unapply(e: And): Option[Seq[FofUnitary]] = Some(e.args)
}
