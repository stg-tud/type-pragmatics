package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

sealed abstract class FofBinaryAssoc(args: Seq[FofUnitary], opString: String) extends Fof {
  require(args.size >= 2, "And/Or should never be created with less than 2 arguments, see the companion objects")

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(args.head)
    args.tail foreach { fu =>
      writer.write(opString)
      writer.write(fu)
    }
  }
}

final class Or private (val args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " | ") {
  override def toString = args.mkString("Or(", ", ", ")")
}
object Or {
  def apply(args: Seq[FofUnitary]): Fof =
    args match {
      case Seq()       => False
      case Seq(single) => single
      case ss           =>
        val flattenedArgs = ss.flatMap(s =>
          if (s.isInstanceOf[Parenthesized] && s.asInstanceOf[Parenthesized].formula.isInstanceOf[Or])
            s.asInstanceOf[Parenthesized].formula.asInstanceOf[Or].args
          else Seq(s))
        new Or(flattenedArgs)
    }

  //    if (args contains True) True
  //    else {
  //      val filteredArgs = args.filterNot {
  //        case False => true
  //        case _     => false
  //      }
  //      filteredArgs match {
  //        case Seq()       => False
  //        case Seq(single) => single
  //        case s           => new Or(s)
  //      }
  //    }

  def unapply(e: Or): Option[Seq[FofUnitary]] = Some(e.args)
}

final case class Xor private (val args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " <~> ") {
  override def toString = args.mkString("Xor(", ", ", ")")
}

final class And private (val args: Seq[FofUnitary]) extends FofBinaryAssoc(args, " & ") {
  override def toString = args.mkString("And(", ", ", ")")
}
object And {
  def apply(args: Seq[FofUnitary]): Fof =
    args match {
      case Seq()       => True
      case Seq(single) => single
      case ss           =>
        val flattenedArgs = ss.flatMap(s =>
          if (s.isInstanceOf[Parenthesized] && s.asInstanceOf[Parenthesized].formula.isInstanceOf[And])
            s.asInstanceOf[Parenthesized].formula.asInstanceOf[And].args
          else Seq(s))
        new And(flattenedArgs)
    }

  //    if (args contains False) False
  //    else {
  //      val filteredArgs = args.filterNot {
  //        case True => true
  //        case _    => false
  //      }
  //      filteredArgs match {
  //        case Seq()       => True
  //        case Seq(single) => single
  //        case s           => new And(s)
  //      }
  //    }

  def unapply(e: And): Option[Seq[FofUnitary]] = Some(e.args)
}
