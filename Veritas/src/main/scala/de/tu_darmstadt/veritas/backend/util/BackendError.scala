package de.tu_darmstadt.veritas.backend.util

/**
 * Custom exception superclass for all errors happening inside the backend.
 * The failure reason may be any type, not just String (hopefully useful for debugging).
 */
class BackendError[+E](val reason: E) extends RuntimeException(reason.toString)

object BackendError {
  def apply(message: String) = new BackendError[String](message)
}