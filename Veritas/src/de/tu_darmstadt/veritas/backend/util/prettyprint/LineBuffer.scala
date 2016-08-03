package de.tu_darmstadt.veritas.backend.util.prettyprint

class LineBuffer(capacity: Int) {
  private val buf = new StringBuilder(capacity)
  private var lineLength = 0
  
  /**
   * Current line length
   * NOTE this is not the same as the current contents of the buffer, since the line might
   * already be partially written and thus empty/shorter then the line!
   */
  def length: Int = lineLength
  
  def ++=(str: String): Unit = {
    buf ++= str
    lineLength += str.length
  }
  
  /**
   * resets the line length to the current buffer content, but does NOT clear the buffer
   */
  def newLine(): Unit = {
    lineLength = buf.length
  }
  
  /**
   * removes the read content from the buffer, but line length stays the same
   * (use it for partially writing this buffer)
   */
  def readAndRemoveContents(): String = {
    val result = buf.toString()
    buf.clear()
    result
  }
  
  def isEmpty: Boolean = length == 0
}