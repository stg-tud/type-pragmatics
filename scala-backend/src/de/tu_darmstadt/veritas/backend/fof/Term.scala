package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable


trait Term extends PrettyPrintable

abstract class Variable(name: String) extends Term with SimplePrettyPrintable {
  override def prettyString = s"V$name" 
}

// NOTE adds the V to make it valid _U_ppercase variable name 
final case class UntypedVariable(name: String) extends Variable(name)

abstract class FunSymbol(name: String) extends Term with SimplePrettyPrintable {
  override def prettyString = s"v$name"
}

// NOTE adds the v to make it valid _l_owercase name for symbols (for functions in fof) 
final case class UntypedFunSymbol(name: String) extends FunSymbol(name)

// TODO what exactly distinguishes PlainTerm from Term? No free variables? No variables at all?
sealed trait PlainTerm extends Term with FofUnitary

final case class Appl(function: FunSymbol, args: Seq[Term]) extends PlainTerm {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(function.prettyString)
    if (!args.isEmpty) {
      writer.write("(")
      writer.write(args.head)
      if (args.tail != Seq())
        args.tail foreach {
          writer.write(", ")
          writer.write(_)
        }
      
      writer.write(")")
    }
  }
}

// NOTE for now, let's model functions just by their name as String (see Appl above)
//  final case class Function(name: String) extends PlainTerm { override def prettyString = name }
