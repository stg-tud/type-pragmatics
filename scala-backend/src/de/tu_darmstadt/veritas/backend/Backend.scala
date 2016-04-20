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
import de.tu_darmstadt.veritas.backend.util.Util

object Backend {

  val fullVariability = FullVariability
  val noGuardedFOF = PartialVariability(Map(FinalEncoding -> Seq(FinalEncoding.BareFOF, FinalEncoding.TFF)))
  val onlyGuardedFOF = PartialVariability(Map(FinalEncoding -> Seq(FinalEncoding.GuardedFOF)))

  val singleTransformation = PartialVariability(
    Map(FinalEncoding -> Seq(FinalEncoding.BareFOF),
      (Problem -> Seq(Problem.Test)),
      (VariableEncoding -> Seq(VariableEncoding.Unchanged)),
      (Simplification -> Seq(Simplification.LogicalAndConstructors))))

  val onlyTFFTest = Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
    Simplification -> Simplification.LogicalAndConstructors,
    VariableEncoding -> VariableEncoding.NameEverything,
    Problem -> Problem.Proof))
  val onlyGuardedFOFTest = Configuration(Map(FinalEncoding -> FinalEncoding.GuardedFOF,
    Simplification -> Simplification.LogicalAndConstructors,
    VariableEncoding -> VariableEncoding.InlineEverything,
    Problem -> Problem.Proof))

  val inliningOnlyTest = PartialVariability(Map(VariableEncoding -> Seq(VariableEncoding.InlineEverything)))
  val inliningComparisonTest = PartialVariability(
    Map(VariableEncoding -> Seq(VariableEncoding.InlineEverything, VariableEncoding.Unchanged),
      Problem -> Seq(Problem.Test)))
  val consistencyOnlyTest = PartialVariability(Map(Problem -> Seq(Problem.Consistency)))

  /**
   * This variability model is used by the code below
   */
  val variabilityModel = fullVariability //runs for at least several minutes!
  //val variabilityModel = onlyGuardedFOFTest

  /**
   * this variability model is used for single verifications
   * triggered from Veritas
   */
  val defaultVariabilityModel = Configuration(
    Map(FinalEncoding -> FinalEncoding.BareFOF,
      (Problem -> Problem.All),
      (VariableEncoding -> VariableEncoding.InlineEverything),
      (Simplification -> Simplification.LogicalAndConstructors)))

  private def writeFile(file: PrettyPrintableFile, path: String): String = {
    val filehandler = new File(path)
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

    path
  }

  type ResultProcessor = (Configuration, Seq[PrettyPrintableFile]) => Seq[(String, PrettyPrintableFile)]

  /**
   * processes results of a single encoding alternative for a given Stratego file, writes result files
   */
  @throws[BackendError[_]]("and the appropriate subclasses on internal error at any stage")
  private def processSingleResult(inputDirectory: String): ResultProcessor = { (config, outputFiles) =>
    Context.log(s"Finished generation for configuration $config\n")

    val problem = config(Problem).toString().toLowerCase
    val typing = config(FinalEncoding).toString().toLowerCase
    val variable = config(VariableEncoding).toString().toLowerCase
    val simpl = config(Simplification).toString().toLowerCase

    val outputFolder = s"$problem/$typing/$variable/$simpl"

    //write the files in the corresponding directory
    //is it necessary to use the Stratego context when backend is called as a strategy
    //when writing the files...?
    outputFiles map { file =>
      val pathname = writeFile(file, s"$inputDirectory/$outputFolder/${file.filename}")
      (pathname, file)
    }
  }

  /**
   * run all encoding alternatives for a single given Stratego file
   * returns pairs of directory name of a file and the actual prettyPrintableFile
   */
  private def runEncodings(
    input: StrategoTerm,
    processResult: ResultProcessor,
    variabilityModel: VariabilityModel = this.variabilityModel): Seq[(String, PrettyPrintableFile)] = {
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
      result = result ++ processResult(config, files)
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
        "Argument must be a triple: (project path (Veritas), input file directory, AST of file as Stratego term)")
    }

    val inputDirectory = s"$projectPath/$inputDir"

    Context.initStrategy(context)

    val resultFiles = runEncodings(ast, processSingleResult(inputDirectory)) //writes files

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
   * Run backend as default strategy from inside Veritas editor with a single default configuration
   */
  def runAsConsistencyStrategy(context: org.strategoxt.lang.Context, inputFromEditor: IStrategoTerm): IStrategoList = {
    // check and destructure input
    val (projectPath, inputDir, proofPath, ast) = StrategoTerm(inputFromEditor) match {
      case StrategoTuple(StrategoString(projectPath), StrategoString(inputDir), StrategoString(proofPath), ast) => (projectPath, inputDir, proofPath, ast)
      case _ => throw new IllegalArgumentException("Illegal input to backend-strategy: " +
        "Argument must be a quatruple: (project path (Veritas), input file directory, proofPath, AST of file as Stratego term)")
    }

    val consistencyFile = s"$projectPath/${Util.removeExtension(proofPath)}-consistency.fof"

    Context.initStrategy(context)

    val proc: ResultProcessor = { (config, outputFiles) =>
      if (outputFiles.size != 1)
        Context.log(s"There should be a single goal encoding, but got ${outputFiles.size} results")

      if (outputFiles.size < 1)
        throw new IllegalStateException(s"There should be a single goal encoding, but got ${outputFiles.size} results")
      else {
        val file = outputFiles.last
        val pathname = writeFile(file, consistencyFile)
        Seq((pathname, file))
      }
    }

    val resultFiles = runEncodings(ast, proc, defaultVariabilityModel)

    // generate return value that is expected by caller of runAsStrategy
    var resseq: Seq[IStrategoTuple] = Seq()

    for ((fullname, file) <- resultFiles) {
      val strategoFilename = context.getFactory.makeString(s"${Util.removeExtension(proofPath)}-consistency.fof")
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

  def runAsProofStrategy(context: org.strategoxt.lang.Context, inputFromEditor: IStrategoTerm): IStrategoList = {
    // check and destructure input
    val (projectPath, inputDir, basePath, ast) = StrategoTerm(inputFromEditor) match {
      case StrategoTuple(StrategoString(projectPath), StrategoString(inputDir), StrategoString(proofPath), ast) => (projectPath, inputDir, proofPath, ast)
      case _ => throw new IllegalArgumentException("Illegal input to backend-strategy: " +
        "Argument must be a quatruple: (project path (Veritas), input file directory, proofPath, AST of file as Stratego term)")
    }

    Context.initStrategy(context)

    val proc: ResultProcessor = { (config, outputFiles) =>
      outputFiles map { file =>
        val filename = Util.generateFileName(basePath, file.goalname)
        val pathname = writeFile(file, s"$projectPath/$filename")
        (filename, file)
      }
    }

    val resultFiles = runEncodings(ast, proc, defaultVariabilityModel)

    var resseq: Seq[IStrategoTuple] = Seq() // Seq[(RuleName, FilePath)]

    for ((fullname, file) <- resultFiles) {
      val strategoGoalname = context.getFactory.makeString(file.goalname)
      val strategoFilename = context.getFactory.makeString(fullname)
      val strategoTuple = context.getFactory.makeTuple(strategoGoalname, strategoFilename)
      resseq = resseq :+ strategoTuple
    }

    if (resseq.isEmpty)
      // return empty list on failure
      context.getFactory.makeList()
    else
      context.getFactory.makeList(resseq.asJava)
  }

  /**
   * function for debugging:
   *
   * apply a single partial transformation chain on a module, without writing files
   * for debugging intermediate steps
   *
   * customize conf and CustomPartialChain below for debugging
   */
  def debugTransformation(aterm: StrategoTerm): Seq[(String, PrettyPrintableFile)] = {

    import de.tu_darmstadt.veritas.backend.transformation._
    import de.tu_darmstadt.veritas.backend.transformation.defs._
    import de.tu_darmstadt.veritas.backend.transformation.imports._
    import de.tu_darmstadt.veritas.backend.transformation.lowlevel._

    val conf = //defaultVariabilityModel
      Configuration(
        Map(FinalEncoding -> FinalEncoding.BareFOF,
          (Problem -> Problem.All),
          (VariableEncoding -> VariableEncoding.InlineEverything),
          (Simplification -> Simplification.LogicalAndConstructors)))

    val modules = Seq(Module.from(aterm))

    val resultingModSeq = MainTrans(modules)(conf)
    val result = resultingModSeq map { m => TypingTrans.finalEncoding(m)(conf) }
    //resultingModSeq map { m => ("", m) }
    result map { m => ("", m) }
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

    Context.initStandalone(indexAndTaskenginePath)

    //use line below for debugging single partial transformation chains
    val resultFiles = debugTransformation(aterm)

    val inputDirectory = "test"
    //    val resultFiles = runEncodings(aterm, processSingleResult(inputDirectory)) //writes files

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Finished all generation")

    // write resulting files on console in addition
    if (resultFiles.size < 100)
      for ((fullname, outputFiles) <- resultFiles) {
        // print a filename header, then the contents
        outputPrettyPrinter.write("File '", outputFiles.filename, "':")
        outputPrettyPrinter.indent().write(outputFiles).unindent()
        outputPrettyPrinter.writeln()
      }
    outputPrettyPrinter.close()
  }
}
