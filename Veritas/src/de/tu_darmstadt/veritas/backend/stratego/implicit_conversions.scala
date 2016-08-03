package de.tu_darmstadt.veritas.backend

import scala.language.implicitConversions

package object stratego {
  implicit def toStrategoInt(i: Int) = StrategoInt(i)
  implicit def toStrategoReal(r: Double) = StrategoReal(r)
  implicit def toStrategoString(s: String) = StrategoString(s)
//  implicit def toStrategoList(seq: Seq[StrategoTerm]) = StrategoList(seq)
}