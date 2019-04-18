package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation

import java.io._
import java.util.Calendar

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenerator}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

class LemmaStore(file: File) {
  def serialize(lemmas: Map[FunctionDef, Seq[Lemma]]): Unit = {
    val stream = new ObjectOutputStream(new FileOutputStream(file))
    stream.writeObject(lemmas)
    stream.close()
  }

  def deserialize(): Map[FunctionDef, Seq[Lemma]] = {
    val stream = new ObjectInputStream(new FileInputStream(file))
    val lemmas = stream.readObject().asInstanceOf[Map[FunctionDef, Seq[Lemma]]]
    stream.close()
    lemmas
  }

  def loadOrGenerate(generator: LemmaGenerator): Map[FunctionDef, Seq[Lemma]] = {
    if(!file.exists()) {
      println(s"generating lemmas with $generator ...")
      println(s"current time: ${Calendar.getInstance().getTime}")
      val lemmas = generator.generateLemmas()
      println(s"writing lemmas to $file ...")
      serialize(lemmas)
      lemmas
    } else {
      println(s"reading lemmas from $file ...")
      deserialize()
    }
  }
}
