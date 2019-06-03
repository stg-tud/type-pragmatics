package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation

import java.io.{File, FileWriter}

import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

/** Some helper methods */
trait EvaluationHelpers {
  def sortFunctions(functions: Seq[FunctionDef]): Seq[FunctionDef] = {
    functions.sortBy(_.signature.name)
  }

  def formatFunctionName(f: FunctionDef): String = {
    s"\\cod{${f.signature.name}}"
  }

  def printToFile(file: File, content: String): Unit = {
    val writer = new FileWriter(file)
    writer.write(content)
    writer.close()
  }

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  def ensureEmpty(file: File): Unit = {
    if(file.exists()) recursivedelete(file)
    file.mkdirs()
  }
}
