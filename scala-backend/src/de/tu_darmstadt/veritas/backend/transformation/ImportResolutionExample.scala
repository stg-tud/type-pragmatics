package de.tu_darmstadt.veritas.backend.transformation

import java.io.PrintWriter

import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.Context
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.veritas.Module

object ImportResolutionExample extends App {
  /* Setup standalone context (i.e. read INDEX and TASKS from persisted files) */
  Context.initStandalone("test/")

  /* Veritas test file */
  val term = StrategoTerm.fromATermFile("test/RecursiveImports.analyzed.aterm");
  val module = Module.from(term);
  
  /* Do it! */
  val pp = new PrettyPrintWriter(new PrintWriter(System.out))
  
  pp.write("The input stratego term:")
  pp.indent()
  pp.writeln(term.toString).writeln()
  pp.unindent()
  
  pp.write("The pretty-printed parsed Veritas Module from that input:")
  pp.indent()
  pp.writeln(module)
  pp.unindent()
  
  pp.write("After resolving the imports:")
  pp.indent()
  val resolved = ImportResolution.resolveImports(module)
  pp.writeln(resolved)
  pp.unindent()
  
  pp.write("After pulling all imports in the top-level")
  pp.indent()
  pp.writeln(ImportResolution.flattenResolvedImports(resolved))
  pp.unindent()
  pp.flush()
}