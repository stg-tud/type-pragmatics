package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
import scala.meta._
import scalafix.v1._

class AddDSKAnnotationTransformer(functionName: String,
                                  annotationName: String,
                                  lemmaName: String) extends Transformer {
  override def apply(tree: Tree): Tree = tree match {

    case node => super.apply(node)
  }
}

class ScalaSPLSpecificationOutput(sourceString: String, lemmas: Map[String, Set[Lemma]]) {
  private val input = Input.VirtualFile("test.scala", sourceString)
  private val doc = SyntacticDocument.fromInput(input)

  def addDSKAnnotation(functionName: String, annotationName: String, lemmaName: String): Unit = {
    val x = doc.tree
      .collect {
        case fn@Defn.Def(mods, nameTerm@Term.Name(name), _, _, _, _) if name == functionName =>
          // we have found ``functionName``, we add an annotation
          val insertionPoint = mods.headOption.getOrElse(fn)
          val lemmaString = "\"" + lemmaName + "\""
          Patch.addLeft(insertionPoint, s"@$annotationName($lemmaString))")
      }.asPatch
    x.showDiff()
  }
  def generate(): String = {
    for((fn, lemmas) <- lemmas) {
      for(lemma <- lemmas) {
        addDSKAnnotation(fn, "hubbabubba", lemma.name)
      }
    }
    "auh"
  }
}
