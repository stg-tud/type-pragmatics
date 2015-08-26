package de.tu_darmstadt.veritas.backend.util.prettyprint

import scala.collection.mutable.Stack

class IndentationStack {
  private val stack = Stack[Indent]()
  
  /*
   * Standard stack interface
   */
  def push(i: Indent): Unit = stack.push(i) // TODO should this automatically make the 2nd-topmost Forced?
  def pop(): Indent = stack.pop()
  
  /**
   * Gives the current indentation as a string (for prepending it to a line).
   * NOTE: OptionalIndent is not printed, only ForcedIndents
   */
  override def toString() = stack.map(_.toString).mkString("")

  /**
   * The number of characters of the current indentation
   */
  def length = toString.length
  
  /**
   * @return true, if the topmost/last added indentation is OptionalIndent
   */
  def isCurrentOptional: Boolean = stack.headOption match {
    case Some(OptionalIndent(_)) => true
    case _ => false
  }
  
  /**
   * Turns the topmost indentation in a forced one
   */
  def setCurrentForced(): Unit = if (!stack.isEmpty) {
    stack.pop() match {
      case OptionalIndent(str) => stack.push(ForcedIndent(str))
      case ForcedIndent(str) => stack.push(ForcedIndent(str))
    }
  }
}

sealed trait Indent
final case class ForcedIndent(indentChars: String) extends Indent { override def toString = indentChars }
final case class OptionalIndent(indentChars: String) extends Indent { override def toString = "" }
