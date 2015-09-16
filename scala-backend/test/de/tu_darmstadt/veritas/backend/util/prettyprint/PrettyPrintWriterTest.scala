package de.tu_darmstadt.veritas.backend.util.prettyprint

import org.scalatest.FunSuite
import java.io.PrintWriter
import java.io.StringWriter

class PrettyPrintWriterTest extends FunSuite {
  test("SimplePrettyPrintable should just output the prettyString") {
    assert(SimpleWidget("bla").toPrettyString == "bla")
  }

  test("write()") {
    val pp = new PrettyPrintWriter
    pp.write("bla")
    assert(pp.toString == "bla")
  }
  
  test("writeln() should append a newline") {
    val pp = new PrettyPrintWriter
    pp.writeln("bla")
    assert(pp.toString == "bla\n")
  }
  
  test("indent() should only write the newline, no indentation until the line is filled with write()") {
    val pp = new PrettyPrintWriter
    pp.indent("  ")
    assert(pp.toString == "\n")
    pp.write("bla")
    assert(pp.toString == "\n  bla")
    // NOTE calling write() again should not indent twice!
    pp.write("")
    assert(pp.toString == "\n  bla")
  }
  
  test("empty lines should have no indentation") {
    val pp = new PrettyPrintWriter
    pp.indent("  ")
    assert(pp.toString == "\n")
    // but non-empty lines should have indentation
    pp.writeln("bla")
    assert(pp.toString == "\n  bla\n")
  }
  
  test("indentation should be prepended to new lines") {
    val pp = new PrettyPrintWriter
    pp.indent("  ")
    assert(pp.toString == "\n")
    pp.write("bla")
    assert(pp.toString == "\n  bla")
  }
  
  test("unindent() should affect the CURRENT line if empty, the NEXT line if sth was written to current") {
    // a) e.g. as in FunctionEq
    val pp1 = new PrettyPrintWriter
    pp1.write("begin")
    assert(pp1.toString == "begin")
    pp1.indent().write("substructure")
    // internally: "begin\n  substructure"
    pp1.unindent()
    // NOTE: no affect of unindent() on current line, instead the indentation there stays "  "
    assert(pp1.toString == "begin\n  substructure")
  
    // Counter example b) e.g. as in Local
    val pp2 = new PrettyPrintWriter
    pp2.write("begin")
    assert(pp2.toString == "begin")
    pp2.indent().writeln("substructure")
    assert(pp2.toString == "begin\n  substructure\n")
    pp2.unindent()
    // internally "begin\n  substructure\n"
    // NOTE no indentation on the empty last line
    pp2.write("end")
    assert(pp2.toString == "begin\n  substructure\nend")
  }
  
  test("trim whitespace at the end of lines") {
    val pp = new PrettyPrintWriter
    pp.writeln("line 1 with space  ")
    pp.writeln("line 2 with tabs\t")
    assert(pp.toString == "line 1 with space\nline 2 with tabs\n")
    // NOTE we cannot trim at the last line (that is flushed without a linebreak, since there might
    // come more content
    pp.write("current line with space  ")
    assert(pp.toString == "line 1 with space\nline 2 with tabs\ncurrent line with space  ")
  }
  
  test("full example test: write, indent, write, undindent, write") {
    val pp = new PrettyPrintWriter
    pp.write("lala")
    assert(pp.toString == "lala")
    pp.indent("  ")
    assert(pp.toString == "lala\n")
    pp.writeln("lulu")
    assert(pp.toString == "lala\n  lulu\n")
    pp.unindent()
    assert(pp.toString == "lala\n  lulu\n")
    pp.write("bla")
    assert(pp.toString == "lala\n  lulu\nbla")
  }

  test("indentOptional() should not print a newline if the following content is short") {
    val pp = new PrettyPrintWriter
    pp.indentOptional().write("bla")
    // NOTE unindent() is needed to finish/flush the optional indent line
    pp.unindent()
    assert(pp.toString == "bla")
  }
  
  test("indentOptional() should print a newline with indentation if the content is longer than the linewidth") {
    val pp = new PrettyPrintWriter(new StringWriter, 5)
    pp.indentOptional().write("abcdefgh")
    // NOTE unindent() is needed to finish/flush the optional indent line
    pp.unindent()
    assert(pp.toString == "\n  abcdefgh")
  }
  
  test("newline after an indentOptional() should make it a forced indent") {
    val pp = new PrettyPrintWriter
    pp.write("begin ")
    pp.indentOptional().writeln("substructure line 1").write("substructure line 2")
    pp.unindent()
    assert(pp.toString == "begin \n  substructure line 1\n  substructure line 2")
  }
  
  test("indentOptionals() after indentOptional() should make both forced indents") {
    val pp = new PrettyPrintWriter
    pp.write("begin ")
    pp.indentOptional().write("level 1 optional")
    pp.indentOptional().write("level 2 optional")
    pp.unindent().unindent()
    assert(pp.toString == "begin \n  level 1 optional\n    level 2 optional")
  }

  // ALTERNATIVE BEHAVIOUR to the one specified in the previous two current tests
//  test("indentOptionals() after indentOptional() should make the second a forced one") {
//    val pp = new PrettyPrintWriter
//    pp.write("begin ")
//    pp.indentOptional().write("level 1 optional")
//    pp.indentOptional().write("level 2 optional")
//    pp.unindent().unindent()
//    assert(pp.toString == "begin level 1 optional\n  level 2 optional")
//  }
}

final case class SimpleWidget(elem: String) extends SimplePrettyPrintable {
  override val prettyString = elem
}

final case class RootWidget(elem: SimpleWidget) extends PrettyPrintable {
  def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("RootWidget(")
    writer.write(elem).write(")")
  }
}
