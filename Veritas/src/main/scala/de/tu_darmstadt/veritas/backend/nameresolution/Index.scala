package de.tu_darmstadt.veritas.backend.nameresolution

import java.io.File

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import org.spoofax.interpreter.library.index.IIndex
import org.spoofax.interpreter.library.index.IndexManager
import org.spoofax.terms.TermFactory

import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm

/** 
 * Wrapper around the Spoofax Java IIndex for for debugging/printing a persisted index.idx file
 * (see PrintIndex object below)
 */
class Index private (spoofaxIndex: IIndex) extends Iterable[(StrategoTerm, StrategoTerm)] {
  // standard Map-like interface
  
  /** 
   * NOTE the Spoofax index can contain more than one matching entry, 
   * but for now we just give back the first one
   */
  def get(key: StrategoTerm): Option[StrategoTerm] =
    spoofaxIndex.get(key.toIStrategoTerm).asScala.headOption.map(firstEntry => StrategoTerm(firstEntry.value))

  
  /** 
   * Iterator over the (key, value) pairs stored in the Index 
   */
  def iterator = 
    spoofaxIndex.getAll.asScala.map(entry => (StrategoTerm(entry.key), StrategoTerm(entry.value))).iterator
  
  // for DEBUGGING
  override def toString = this.iterator.mkString("\n")
}

object Index {
  /**
   * Convenience Ctor: read index from filename 
   */
  def fromFile(indexFilename: String) = 
    new Index(IndexManager.getInstance().read(new File(indexFilename), new TermFactory()))
  
  /**
   * Convenience Ctor: use the current Index (useful when executing from Spoofax)
   */
  def current(): Index = new Index(IndexManager.getInstance.getCurrent())
}

object PrintIndex extends App {
  println(Index.fromFile("test/.cache/index.idx"))
}