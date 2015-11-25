package de.tu_darmstadt.veritas.backend

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.control.NonFatal

import org.spoofax.interpreter.terms.IStrategoList
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.interpreter.terms.IStrategoTuple

import de.tu_darmstadt.veritas.backend.Configuration._
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

object Backend {

  val fullVariability = FullVariability
  val noGuardedFOF = PartialVariability(Map(FinalEncoding -> Seq(FinalEncoding.BareFOF, FinalEncoding.TFF)))
  val onlyGuardedFOF = PartialVariability(Map(FinalEncoding -> Seq(FinalEncoding.GuardedFOF)))

  val singleTransformation = PartialVariability(
      Map(FinalEncoding -> Seq(FinalEncoding.TFF),
      (Problem -> Seq(Problem.Consistency)),
      (InversionLemma -> Seq(InversionLemma.On)),
      (VariableEncoding -> Seq(VariableEncoding.Unchanged)),
      (LogicalSimplification -> Seq(LogicalSimplification.On))))

  val onlyTFFTest = Configuration(Map(FinalEncoding -> FinalEncoding.TFF, LogicalSimplification -> LogicalSimplification.On, VariableEncoding -> VariableEncoding.Unchanged, InversionLemma -> InversionLemma.On, Problem -> Problem.Test))
  val onlyGuardedFOFTest = Configuration(Map(FinalEncoding -> FinalEncoding.GuardedFOF, LogicalSimplification -> LogicalSimplification.On, VariableEncoding -> VariableEncoding.Unchanged, InversionLemma -> InversionLemma.On, Problem -> Problem.Test))
 
  
  /**
   * This variability model is used by the code below
   */
  val variabilityModel = onlyGuardedFOFTest
  
  
  
  private var inputDirectory: String = "" //directory of input file

  private def writeFile(file: PrettyPrintableFile, outputfolder: String): String = {
    val pathname = s"$inputDirectory/$outputfolder/${file.filename}"
    val filehandler = new File(pathname)
    Context.log(s"Writing file to ${filehandler.getAbsolutePath}...")
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    val bw = new FileOutputStream(filehandler);
    try {
      bw.write(file.toPrettyString().getBytes())
    } finally {
      bw.close()
    }

    pathname
  }

  /**
   * runs a single encoding alternative for a given Stratego file
   */
  @throws[BackendError[_]]("and the appropriate subclasses on internal error at any stage")
  private def processSingleResult(config: Configuration, outputFiles: Seq[PrettyPrintableFile]): Seq[(String, PrettyPrintableFile)] = {
    Context.log(s"Finished generation for configuration $config\n")

    val problem = config(Problem).toString().toLowerCase
    val typing = config(FinalEncoding).toString().toLowerCase
    val variable = config(VariableEncoding).toString().toLowerCase
    val simpl = config(LogicalSimplification).toString().toLowerCase
    val inv = config(InversionLemma).toString().toLowerCase
    
    val outputFolder = s"$problem/$typing/$variable-$simpl-$inv"
    
    //write the files in the corresponding directory
    //is it necessary to use the Stratego context when backend is called as a strategy
    //when writing the files...?
    outputFiles map { file =>
      val pathname = writeFile(file, outputFolder)
      (pathname, file)
    }
  }

  /**
   * run all encoding alternatives for a single given Stratego file
   * returns pairs of directory name of a file and the actual prettyPrintableFile
   */
  private def runAllEncodings(input: StrategoTerm): Seq[(String, PrettyPrintableFile)] = {
    val module = Module.from(input)
    val comparison = new EncodingComparison(variabilityModel, module)

    var result: Seq[(String, PrettyPrintableFile)] = Seq()
    val it = comparison.iterator
    while (it.hasNext) {
      val (config, files) = try {
        it.next()
      } catch {
        case NonFatal(e) =>
          Context.log(s"FAILED: Generation for configuration ${comparison.lastConfig}:")
          Context.reportException(e.asInstanceOf[Exception])
          (comparison.lastConfig, Seq())
      }
      result = result ++ processSingleResult(config, files)
    }
    
    result
  }

  /**
   * Run backend as strategy from inside Veritas editor
   */
  def runAsStrategy(context: org.strategoxt.lang.Context, inputFromEditor: IStrategoTerm): IStrategoList = {
    // check and destructure input
    val (projectPath, inputDir, ast) = StrategoTerm(inputFromEditor) match {
      case StrategoTuple(StrategoString(projectPath), StrategoString(inputDir), ast) => (projectPath, inputDir, ast)
      case _ => throw new IllegalArgumentException("Illegal input to backend-strategy: " +
        "Argument must be a tuple: (input file directory, AST of file as Stratego term)")
    }

    inputDirectory = s"$projectPath/$inputDir"

    Context.initStrategy(context)

    val resultFiles = runAllEncodings(ast) //writes files

    // generate return value that is expected by caller of runAsStrategy
    var resseq: Seq[IStrategoTuple] = Seq()

    for ((fullname, file) <- resultFiles) {
      val strategoFilename = context.getFactory.makeString(fullname)
      val strategoContent = context.getFactory.makeString(file.toPrettyString)
      val strategoTuple = context.getFactory.makeTuple(strategoFilename, strategoContent)
      resseq = resseq :+ strategoTuple
    }

    if (resseq.isEmpty)
      // return empty list on failure
      context.getFactory.makeList()
    else
      context.getFactory.makeList(resseq.asJava)
  }

  /**
   * Run backend as console application, with optional arguments for giving the ATerm and Index/Tasks
   */
  val DefaultATerm = "test/Semantics-test.analyzed.aterm"
  val DefaultIndexAndTaskenginePath = "../Veritas/"

  def main(args: Array[String]) {
    val (aterm, indexAndTaskenginePath) = args match {
      case Array()                                      => (StrategoTerm.fromATermFile(DefaultATerm), DefaultIndexAndTaskenginePath)
      case Array(atermFilename)                         => (StrategoTerm.fromATermFile(atermFilename), DefaultIndexAndTaskenginePath)
      case Array(atermFilename, indexAndTaskenginePath) => (StrategoTerm.fromATermFile(atermFilename), indexAndTaskenginePath)
    }

    //set default directory for output files
    inputDirectory = "test"

    Context.initStandalone(indexAndTaskenginePath)

    //use this to debug a single partial transformation....
    //val resultFiles = Seq(("", (BasicTrans(Seq(Module.from(aterm)))(Configuration(Map()))).head))
    
    val resultFiles = runAllEncodings(aterm) //writes files

    // write resulting files on console in addition
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    for ((fullname, outputFiles) <- resultFiles) {
      // print a filename header, then the contents
      outputPrettyPrinter.write("File '", outputFiles.filename, "':")
      outputPrettyPrinter.indent().write(outputFiles).unindent()
      outputPrettyPrinter.writeln()
    }
    outputPrettyPrinter.flush()
  }
}
