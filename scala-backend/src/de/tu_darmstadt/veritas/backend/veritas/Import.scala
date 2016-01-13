package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.nameresolution.NameResolution
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

/**
 * Imports of other Veritas modules. 
 * Spoofax does imports by annotating the import AST node with a "NaBL Use". These Uses can have
 * different forms, i.e. a "Task ID" (such as "Use(Result(<someInteger>))") or a Module URI.  
 */

sealed trait Import extends VeritasConstruct with SimplePrettyPrintable {
  private var _importAnnotations: Seq[ImportAnnotation] = Seq()
  def importAnnotations = _importAnnotations
  def importAnnotationsPretty = 
    if (importAnnotations.isEmpty) "" 
    else importAnnotations.map(_.prettyString).mkString("{", ", ", "} ")
    
  /* abstract */ def moduleName: String
  
  /** Equality for imports means equal imported Module name (ignore all other fields, whether it
   *  is resolved or not etc. 
   */ 
  override def equals(o: Any) = o match {
    case that: Import => that.moduleName == this.moduleName 
    case _ => false 
  }
  override def hashCode = this.moduleName.hashCode

  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    this
  }
}

case class Resolved(moduleCode: Module) extends Import {
  override def moduleName = moduleCode.name
  override def prettyString = s"import $moduleName $importAnnotationsPretty// status: resolved" 
}

case class Unresolved(moduleName: String, nablAnnotation: StrategoTerm) extends Import {
  override def prettyString = s"import $moduleName $importAnnotationsPretty// status: unresolved, NaBL annotation: $nablAnnotation"

  def resolve() = Resolved(NameResolution.getModuleDef(this)
                            .getOrElse(throw BackendError(s"import $this could not be resolved")))
}

object Import {
  def from(term: StrategoTerm): Import = term match {
    case i@StrategoAppl("Import", module@StrategoString(moduleName), annos) => {
      val importAnnotations = annos match {
        case StrategoAppl("None") => Seq()
        case StrategoAppl("Some", StrategoAppl("ImportAnno", StrategoList(annos))) => annos map ImportAnnotation.from
        case t => throw VeritasParseError("Import annotation is wrong, must be either None or Some(ImportAnno([...])), got: " + t + "\nin import: "+i)
      }
      
      val result: Import = module.getFirstAnnotation match {
        case Some(nablAnnotation @ StrategoAppl("Use", _)) 
          => Unresolved(moduleName, nablAnnotation)
        case t => throw VeritasParseError("Import (NaBL) annotation is wrong. Must be a NaBL Use(...) with either task ID or module URL, got: " + t + "\nin import: "+i+"\nConsider running Syntax -> Show Analyzed Syntax, this sometimes triggers the generation of NaBl annotations.")
      }
      result._importAnnotations = importAnnotations.distinct
      result
    }
    case t => throw VeritasParseError(t)
  }
}
