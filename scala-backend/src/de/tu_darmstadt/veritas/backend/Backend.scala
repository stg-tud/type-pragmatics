package de.tu_darmstadt.veritas.backend

import java.io.PrintWriter
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.spoofax.interpreter.terms.IStrategoList
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.interpreter.terms.IStrategoTuple
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
import de.tu_darmstadt.veritas.backend.transformation.lowlevel._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import java.io.FileOutputStream

object Backend {

  private var inputDirectory: String = "" //directory of input file
  private var study = new EncodingComparisonStudy

  private def writeFile(file: PrettyPrintableFile, outputfolder: String): String = {
    val pathname = s"$inputDirectory/$outputfolder/${file.filename}"
    val filehandler = new File(pathname)
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
  private def runSingleEncoding(input: StrategoTerm): Seq[(String, PrettyPrintableFile)] = {
    val mod = Module.from(input)

    Context.log(s"Starting generation for encoding ${study.encodingStrategies.head._1}")

    val result = Try {
      study.currEncoding(mod)
    }

    result match {
      case Failure(ex) => {
        Context.log(s"FAILED: Generation for encoding ${study.encodingStrategies.head._1}: ")
        Context.debug(ex)
        // skip files that don't work 
        Seq()
      }

      case Success((outputFolder, outputFiles)) => {

        //write the files in the corresponding directory
        //is it necessary to use the Stratego context when backend is called as a strategy
        //when writing the files...?
        outputFiles map { file =>
          val pathname = writeFile(file, outputFolder)
          (pathname, file)
        }

      }

    }

    // NOTE without the "Out", calling the Strategy from Spoofax fails, because it would overwrite
    // the original file!
    //Seq(Module(mod.name + "Out", mod.imports, mod.body))

  }

  /**
   * run all encoding alternatives for a single given Stratego file
   * returns pairs of directory name of a file and the actual prettyPrintableFile
   */
  private def runAllEncodings(input: StrategoTerm): Seq[(String, PrettyPrintableFile)] = {
    study = new EncodingComparisonStudy

    (for (i <- (0 until study.encodingnum)) yield {
      val result = runSingleEncoding(input)
      study.moveToNextEncoding()
      result
    }).flatten

  }

  /**
   * Run backend as strategy from inside Veritas editor
   */
  def runAsStrategy(context: org.strategoxt.lang.Context, inputFromEditor: IStrategoTerm): IStrategoList = {
    // check and destructure input
    val (inputDir, ast) = StrategoTerm(inputFromEditor) match {
      case StrategoTuple(StrategoString(inputDir), ast) => (inputDir, ast)
      case _ => throw new IllegalArgumentException("Illegal input to backend-strategy: " +
        "Argument must be a tuple: (input file directory, AST of file as Stratego term)")
    }

    inputDirectory = inputDir

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
