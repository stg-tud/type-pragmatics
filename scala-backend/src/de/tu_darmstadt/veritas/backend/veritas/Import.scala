package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.nameresolution.nabl.VeritasModuleUri
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoInt
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.util.Context
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.nameresolution.NameResolution

/**
 * Imports of other Veritas modules. 
 * Spoofax does imports by annotating the import AST node with a "NaBL Use". These Uses can have
 * different forms, i.e. a "Task ID" (such as "Use(Result(<someInteger>))") or a Module URI.  
 */

sealed trait Import extends SimplePrettyPrintable {
  /* abstract */ def moduleName: String
}

sealed trait Unresolved extends Import {
  /* abstract */ def toNablUse: StrategoTerm
  def resolve() = Resolved(NameResolution.getModuleDef(this)
                            .getOrElse(throw BackendError(s"import $this could not be resolved")))
}

// an import is processed in multiple stages, which are (from top to bottom):
case class UnresolvedTaskId(moduleName: String, taskId: Int) extends Unresolved {
  override def toNablUse = StrategoAppl("Use", StrategoAppl("Result", taskId))
  override protected val prettyString = s"import $moduleName // status: unresolved Task ID $taskId"
}

case class UnresolvedUri(moduleUri: VeritasModuleUri) extends Unresolved {
  override def moduleName = moduleUri.name
  override def toNablUse = moduleUri.toNablProperty
  override protected val prettyString = s"import $moduleName // status: unresolved URI $moduleUri"
}

case class Resolved(moduleCode: Module) extends Import {
  override def moduleName = moduleCode.name
  override protected val prettyString = s"import $moduleName // status: resolved" 
}

object Import {
  def from(term: StrategoTerm): Import = term match {
    case StrategoAppl("Import", module@StrategoString(moduleName), _ /* TODO (Veritas, not NaBL) import annotations */) => {
      module.getFirstAnnotation match {
        case Some(StrategoAppl("Use", StrategoAppl("Result", StrategoInt(taskId)))) 
          => UnresolvedTaskId(moduleName, taskId)
        case Some(StrategoAppl("Use", urlTerm)) 
          => UnresolvedUri(VeritasModuleUri.fromNablDef(urlTerm))
        case t => throw VeritasParseError("Import() annotation is wrong. Must be a NaBL Use(...) with either task ID or module URL, got: " + t)
      }
    }
    case t => throw VeritasParseError(t)
  }
}
