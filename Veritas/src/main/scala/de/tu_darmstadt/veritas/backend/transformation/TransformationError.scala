package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.util.BackendError

// NOTE the wrapped error reason is of type T so that we can put any kind of Term format in 
// there, i.e. a Module or just a String...
case class TransformationError[T](input: T) extends BackendError[T](input)