package de.tu_darmstadt.veritas.backend.nameresolution.nabl

import org.spoofax.interpreter.terms.IStrategoTerm

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.BackendError

/**
 * A special case of a NaBL URI, namely for Veritas modules
 */
class VeritasModuleUri(val name: String, qualifier: String) 
  extends NablUri(
    "Veritas", // language
    "NablNsModule", // namespace
    name, 
    qualifier) {
  def toNablProperty: StrategoTerm = this.toNablProperty("NablProp_module-ref")  
}

object VeritasModuleUri {
  /**
   * Parses a NaBL Def (the StrategoTerm), you get this from a Nabl Def Annotation in the AST 
   */
  def fromNablDef(in: StrategoTerm) = in match {
    case StrategoAppl("Def",
           StrategoAppl("URI", _, 
             StrategoList(Seq(
               StrategoAppl("ID", _, StrategoString(moduleName), 
                                     StrategoAppl("Unique", StrategoString(moduleUrl))
                           )
             ))
           )
         ) => new VeritasModuleUri(moduleName, moduleUrl)
    case _ => throw BackendError("exptected a StrategoTerm ala Def(URI(Language(\"Veritas\", ... but got: " + in)
  } 
}
