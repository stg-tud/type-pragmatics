package de.tu_darmstadt.veritas.backend.stratego

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scala.io.Source

import org.spoofax.interpreter.terms.IStrategoAppl
import org.spoofax.interpreter.terms.IStrategoInt
import org.spoofax.interpreter.terms.IStrategoList
import org.spoofax.interpreter.terms.IStrategoReal
import org.spoofax.interpreter.terms.IStrategoString
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.interpreter.terms.IStrategoTuple
import org.spoofax.terms.TermFactory
import org.spoofax.terms.io.TAFTermReader

/**
 * A Scala abstraction over the Java/Stratego IStrategoTerm interface.
 * Motivation: Better/more idiomatic destructuring during parsing
 */
sealed trait StrategoTerm {
  private var _annotations: Seq[StrategoTerm] = Seq()
  def getAnnotations: Seq[StrategoTerm] = _annotations
  def getFirstAnnotation: Option[StrategoTerm] = getAnnotations.headOption
  
  /**
   * Convert back to a IStrategoTerm for interfacing with Java Stratego code
   */
  def toIStrategoTerm: IStrategoTerm = _javaTerm getOrElse {
    import scala.collection.JavaConverters._
    val javaAnnotations = StrategoTerm.factory.makeList(getAnnotations.map(_.toIStrategoTerm).asJava)
    StrategoTerm.factory.annotateTerm(toIStrategoTermNoAnnotations, javaAnnotations)
  }

  // NOTE uses the original IStrategoTerm if it was converted from a Java term
  private var _javaTerm: Option[IStrategoTerm] = None
  
  /* abstract */ protected def toIStrategoTermNoAnnotations: IStrategoTerm

  /**
   * for debugging, should be similar to the IStrategoTerm.toString
   */
  override def toString = toStringWithoutAnnotations + (if (getAnnotations.isEmpty) "" 
                                                  else getAnnotations.mkString(" {", ",", "}"))

  /* abstract */ protected def toStringWithoutAnnotations: String
}

final case class StrategoInt(n: Int) extends StrategoTerm {
  protected def toStringWithoutAnnotations = n.toString
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeInt(n)
}

final case class StrategoReal(r: Double) extends StrategoTerm {
  protected def toStringWithoutAnnotations = r.toString
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeReal(r)
}

final case class StrategoString(s: String) extends StrategoTerm {
  protected def toStringWithoutAnnotations = '"' + s + '"'
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeString(s)
}

final case class StrategoAppl(name: String, children: StrategoTerm*) extends StrategoTerm {
  protected def toStringWithoutAnnotations = name + children.mkString("(", ", ", ")")
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeAppl(
    StrategoTerm.factory.makeConstructor(name, children.size), 
    (children map (_.toIStrategoTerm)) :_*)
}

final case class StrategoList(elements: Seq[StrategoTerm]) extends StrategoTerm {
  protected def toStringWithoutAnnotations = elements.mkString("[", ", ", "]")
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeList(
    (elements map (_.toIStrategoTerm)) :_*)
}

final case class StrategoTuple(elements: StrategoTerm*) extends StrategoTerm {
  protected def toStringWithoutAnnotations = elements.mkString("(", ", ", ")")
  protected def toIStrategoTermNoAnnotations = StrategoTerm.factory.makeTuple(
    (elements map (_.toIStrategoTerm)) :_*)
}


object StrategoTerm {
  private[stratego] val factory = new TermFactory()
  
  /**
   * Convert a Java IStrategoTerm to (home-grown) Scala StrategoTerm
   */
  def apply(input: IStrategoTerm): StrategoTerm = {
    val result = input match {
      // non recursive cases
      case integer: IStrategoInt   => StrategoInt(integer.intValue)
      case real: IStrategoReal     => StrategoReal(real.realValue)
      case string: IStrategoString => StrategoString(string.stringValue)

      // recursive cases
      case appl: IStrategoAppl => {
        val children = appl.getAllSubterms map apply;
        // _* is to explicitly make this a varargs call
        StrategoAppl(appl.getConstructor.getName, children: _*)
      }
      case list: IStrategoList   => StrategoList(list.getAllSubterms map apply)
      case tuple: IStrategoTuple => StrategoTuple(tuple.getAllSubterms map apply: _*)

      // Placeholder/Ref/Blob are additional term types, but they are never used by us

      case t => throw StrategoTermParseError(t)
    }
    // also convert all annotation terms
    result._annotations = input.getAnnotations.getAllSubterms map apply
    result._javaTerm = Some(input)
    result
  }
  
  /**
   * Convenience ctor
   */
  def fromATermFile(filename: String): StrategoTerm = {
    val atermFile = Source.fromFile(filename)
    
    /* StrategoXT throws ParseError on input files with CRLF line endings
     * WORKAROUND explicitly replace all line endings with LF (these are handled in TAFTermReader, line 291)
     */
    // NOTE source.mkString will use CRLF on Windows, so we need mkString("\n")
    val stringLf = atermFile.getLines().mkString("\n")
    val atermFileLinuxEndings = new ByteArrayInputStream(stringLf.getBytes(StandardCharsets.UTF_8))
    
    // try to parse that input file as a IStrategoTerm
    val strategoTerm = new TAFTermReader(new TermFactory()).parseFromStream(atermFileLinuxEndings)
    
    // convert from Java IStrategoTerm to Scala case class and return
    StrategoTerm(strategoTerm)
  }
}