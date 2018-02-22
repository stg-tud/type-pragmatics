package de.tu_darmstadt.veritas.newinputdsl

case class SPLTranslationError(msg: String) extends Error {
  def this(msg: String, cause: Throwable) {
    this(msg)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }
}
