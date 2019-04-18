package de.tu_darmstadt.veritas.scalaspl.translator

case class ScalaSPLTranslationError(msg: String) extends Error(msg) {
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
