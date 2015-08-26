package de.tu_darmstadt.veritas.backend.nameresolution.nabl

import org.spoofax.interpreter.terms.IStrategoString
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.terms.TermFactory
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm

/**
 * Holds NaBL URIs used to link Defs and Uses, e.g. when importing Modules or the like 
 * modeled after runtime/nabl/uri.str
 */
class NablUri(language: String, namespace: String, name: String, qualifier: String) {
  override def toString = this.toStrategoTerm.toString

  // implicit create StrategoString("<some string>")
  import de.tu_darmstadt.veritas.backend.stratego.toStrategoString

  /**
   * Convert to a StrategoTerm that can e.g. be put in the annotation of the construct in a Veritas
   * AST. 
   */
  def toStrategoTerm: StrategoTerm = {
    val segment: StrategoAppl = StrategoAppl("ID", StrategoAppl(namespace), name, StrategoAppl("Unique", qualifier))
    StrategoAppl("URI", StrategoAppl("Language", language), StrategoList(Seq(segment)))
  }
  
  /** 
   * Useful to obtain a NaBL property that can be used to query the Spoofax Index, 
   */
  // NOTE it is overridden in the module URI subclass that gives the correct property name
  protected def toNablProperty(propertyName: String): StrategoTerm = {
    StrategoAppl("Prop", this.toStrategoTerm, StrategoAppl(propertyName))
  }
}
