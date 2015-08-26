package de.tu_darmstadt.veritas.backend.stratego

import org.spoofax.interpreter.terms.IStrategoTerm
import de.tu_darmstadt.veritas.backend.util.BackendError

case class StrategoTermParseError(input: IStrategoTerm) extends BackendError[IStrategoTerm](input)
