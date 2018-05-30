package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslationError

trait Reporter {
  def report[T](msg: String, line: Int): T = {
    throw new ScalaSPLTranslationError(s"Error($line): $msg")
  }

  def report[T](msg: String): T = {
    throw new ScalaSPLTranslationError(msg)
  }
}

object Reporter extends {
  def apply(): Reporter = new Reporter {}
}