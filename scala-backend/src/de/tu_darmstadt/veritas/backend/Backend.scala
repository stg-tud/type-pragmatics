package de.tu_darmstadt.veritas.backend

import java.io.PrintWriter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.spoofax.interpreter.terms.IStrategoList
import org.spoofax.interpreter.terms.IStrategoTerm
import de.tu_darmstadt.veritas.backend.nameresolution.NameResolution
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoTuple
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.Context
import de.tu_darmstadt.veritas.backend.util.Context.debug
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintableFile
import de.tu_darmstadt.veritas.backend.util.stacktraceToString
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.transformation.ToFof
import de.tu_darmstadt.veritas.backend.transformation.ToTff
import de.tu_darmstadt.veritas.backend.transformation.basic.DesugarLemmas

object Backend {
  
  @throws[BackendError[_]]("and the appropriate subclasses on internal error at any stage")
  private def run(input: StrategoTerm): Seq[PrettyPrintableFile] = {
    val mod = Module.from(input)

//    debug("Imported modules:")
//    val res = for {
//      imp <- mod.imports
//      resolved <- NameResolution.getModuleDef(imp)
//    } {
//      debug(resolved)
//      resolved
//    }

//    Seq()
    
//    ToFof.toFofFiles(mod)

//    Seq(PrettyPrintableFile("debug.out", "just a test"))

    // NOTE without the "Out", calling the Strategy from Spoofax fails, because it would overwrite
    // the original file!
    //Seq(Module(mod.name + "Out", mod.imports, mod.body))
    
    // TODO remove these lines as soon as tff pretty printing is complete and works
    // TODO maybe rethink design of fof and tff classes? currently, it's easily possible to 
    // produce fof with typed variables! (provided package tff is imported!)
    
    import de.tu_darmstadt.veritas.backend.fof._
    import de.tu_darmstadt.veritas.backend.tff._
    
    val student = TypedSymbol("student", DefinedType("tType"))
    val student_type = TffAnnotated("student_type", Type, student)
    
    val professor = TypedSymbol("professor", DefinedType("tType"))
    val professor_type = TffAnnotated("professor_type", Type, professor)
    
    val course = TypedSymbol("course", DefinedType("tType"))
    val course_type = TffAnnotated("course_type", Type, course)
    
    val michael = TypedSymbol("michael", SymbolType(student))
    val michael_type = TffAnnotated("michael_type", Type, michael)
    
    val victor = TypedSymbol("victor", SymbolType(professor))
    val victor_type = TffAnnotated("victor_type", Type, victor)
    
    val csc410 = TypedSymbol("csc410", SymbolType(course))
    val csc410_type = TffAnnotated("csc410_type", Type, csc410)
    
    val enrolled = TypedSymbol("enrolled", TffMappingType(Seq(SymbolType(student), SymbolType(course)), DefinedType("o")))
    val enrolled_type = TffAnnotated("enrolled_type", Type, enrolled)
    
    val teaches = TypedSymbol("teaches", TffMappingType(Seq(SymbolType(professor), SymbolType(course)), DefinedType("o")))
    val teaches_type = TffAnnotated("teaches_type", Type, teaches)
    
    val taughtby = TypedSymbol("taughtby", TffMappingType(Seq(SymbolType(student), SymbolType(professor)), DefinedType("o")))
    val taught_by_type = TffAnnotated("taught_by_type", Type, taughtby)
    
    val coordinatorof = TypedSymbol("coordinatorof", TffMappingType(Seq(SymbolType(course)), SymbolType(professor)))
    val coordinator_of_type = TffAnnotated("coordinator_of_type", Type, coordinatorof)
    
    val xvar1 = TypedVariable("X", SymbolType(student))
    val yvar1 = TypedVariable("Y", SymbolType(course))
    val ex1 = Exists(Seq(yvar1), Appl(enrolled.toUntyped, xvar1.toUntyped, yvar1.toUntyped))
    val fa1 = ForAll(Seq(xvar1), ex1)
    val student_enrolled_axiom = TffAnnotated("student_enrolled_axiom", Axiom, fa1)
    
    val xvar2 = TypedVariable("X", SymbolType(professor))
    val yvar2 = TypedVariable("Y", SymbolType(course))
    val ex2 = Exists(Seq(yvar2), Appl(teaches.toUntyped, xvar2.toUntyped, yvar2.toUntyped))
    val fa2 = ForAll(Seq(xvar2), ex2)
    val professor_teaches_axiom = TffAnnotated("professor_teaches_axiom", Axiom, fa2)
    
    val xvar3 = TypedVariable("X", SymbolType(course))
    val yvar3 = TypedVariable("Y", SymbolType(student))
    val ex3 = Exists(Seq(yvar3), Appl(enrolled.toUntyped, yvar3.toUntyped, xvar3.toUntyped))
    val fa3 = ForAll(Seq(xvar3), ex3)
    val course_enrolled = TffAnnotated("course_enrolled", Axiom, fa3)
 
    val xvar4 = TypedVariable("X", SymbolType(course))
    val yvar4 = TypedVariable("Y", SymbolType(professor))
    val ex4 = Exists(Seq(yvar4), Appl(teaches.toUntyped, yvar4.toUntyped, xvar4.toUntyped))
    val fa4 = ForAll(Seq(xvar4), ex4)
    val course_teaches = TffAnnotated("course_teaches", Axiom, fa4)

    val xvar5 = TypedVariable("X", SymbolType(course))
    val fa5 = ForAll(Seq(xvar5), Appl(teaches.toUntyped, Appl(coordinatorof.toUntyped, xvar5.toUntyped), xvar5.toUntyped))
    val coordinator_teaches = TffAnnotated("coordinator_teaches", Axiom, fa5)
    
    val xvar6 = TypedVariable("X", SymbolType(student))
    val yvar6 = TypedVariable("Y", SymbolType(course))
    val zvar6 = TypedVariable("Z", SymbolType(professor))
    val fa6 = ForAll(Seq(zvar6), Parenthesized(Impl(Appl(teaches.toUntyped, zvar6.toUntyped, yvar6.toUntyped), Appl(taughtby.toUntyped, xvar6.toUntyped, zvar6.toUntyped))))
    val fa7 = ForAll(Seq(xvar6, yvar6), Parenthesized(Impl(Appl(enrolled.toUntyped, xvar6.toUntyped, yvar6.toUntyped), fa6)))
    val student_enrolled_taught = TffAnnotated("student_enrolled_taught", Axiom, fa7)

    val michael_enrolled_csc410_axiom = TffAnnotated("michael_enrolled_csc410_axiom", Axiom, 
        Appl(enrolled.toUntyped, michael.toUntyped, csc410.toUntyped))

    val victor_coordinator_csc410_axiom = TffAnnotated("victor_coordinator_csc410_axiom", Axiom,
        Eq(Appl(coordinatorof.toUntyped, csc410.toUntyped), victor.toUntyped)) 
      
    val teaching_conjecture = TffAnnotated("teaching_conjecture", Conjecture, 
        Appl(taughtby.toUntyped, michael.toUntyped, victor.toUntyped))
        
    
    val tffs = Seq[TffAnnotated](student_type, professor_type, course_type, michael_type, 
        victor_type, csc410_type, enrolled_type, teaches_type, taught_by_type, coordinator_of_type,
        student_enrolled_axiom, professor_teaches_axiom, course_enrolled, course_teaches, coordinator_teaches,
        student_enrolled_taught, michael_enrolled_csc410_axiom,
        victor_coordinator_csc410_axiom, teaching_conjecture)
    
    Seq(DesugarLemmas(Module(mod.name + "Out", mod.imports, mod.body))(0), 
        TffFile("Victor_teaches_Michael", tffs))
  }

  /**
   * Run backend as strategy from inside Veritas editor
   */
  def runAsStrategy(context: org.strategoxt.lang.Context, inputFromEditor: IStrategoTerm): IStrategoList = {
    // check and destructure input
    val (inputDirectory, ast) = StrategoTerm(inputFromEditor) match {
      case StrategoTuple(StrategoString(inputDirectory), ast) => (inputDirectory, ast)
      case _ => throw new IllegalArgumentException("Illegal input to backend-strategy: " + 
                "Argument must be a tuple: (input file directory, AST of file as Stratego term)")
    }
    
    Context.initStrategy(context)
    
    // NOTE we need to capture exceptions with Try, so we can print the full stack trace below
    // (otherwise Stratego will silence the stack trace...)
    val backendResult = Try(run(ast))
    
    backendResult match {
      case Failure(ex) => {
        context.getIOAgent.printError(stacktraceToString(ex))
        // return empty list on failure
        context.getFactory.makeList()
      }
      
      case Success(outputFiles) => {
        val scalaSeq = for {
          outputFile <- outputFiles
          // map files to IStrategoTuples with (filename, contents)
          filename = context.getFactory.makeString(inputDirectory + "/" + outputFile.filename)
          content = context.getFactory.makeString(outputFile.toPrettyString)
        } yield context.getFactory.makeTuple(filename, content)
        // convert Scala Seq to Spoofax IStrategoList
        context.getFactory.makeList(scalaSeq.asJava)
      }
    }
  }


  /**
   * Run backend as console application, with optional arguments for giving the ATerm and Index/Tasks
   */
  val DefaultATerm = "test/TableAux.noimports.aterm"
  val DefaultIndexAndTaskenginePath = "test/"
  
  def main(args: Array[String]) {
    val (aterm, indexAndTaskenginePath) = args match {
      case Array() => (StrategoTerm.fromATermFile(DefaultATerm), DefaultIndexAndTaskenginePath)
      case Array(atermFilename) => (StrategoTerm.fromATermFile(atermFilename), DefaultIndexAndTaskenginePath)
      case Array(atermFilename, indexAndTaskenginePath) 
        => (StrategoTerm.fromATermFile(atermFilename), indexAndTaskenginePath)
    }
    
    Context.initStandalone(indexAndTaskenginePath)
    
    // call the strategy on the IStrategoTerm
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    for (outputFiles <- run(aterm)) {
      // print a filename header, then the contents
      outputPrettyPrinter.write("File '", outputFiles.filename, "':")
      outputPrettyPrinter.indent().write(outputFiles).unindent()
      outputPrettyPrinter.writeln()
    }
    outputPrettyPrinter.flush()
  }
}
