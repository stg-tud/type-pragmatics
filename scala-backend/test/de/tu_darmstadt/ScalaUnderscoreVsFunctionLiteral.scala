package de.tu_darmstadt

/*
 * This is an example for unintuitive behavior of Scala underscore syntax (used for partial 
 * application etc.) vs "normal" lambda syntax.
 * 
 * The behavior of the code below "depends" on the syntax of the anonymous function given to the 
 * foreach. (the second example is simply erroneous, but it was tricky to see for me).
 * 
 * I wrongly used the second syntax when pretty printing a Seq[Variable] in FofUnitary, which 
 * resulted in a BUG that there were no commas in the variable list. 
 */
object ScalaUnderscoreVsFunctionLiteral extends App {
  
  // We want to prepend every element in the list with a '-'
  val l = List("a", "b", "c")
  
  // 1) Use normal lambda syntax
  val s1 = new StringBuilder
  l foreach { x => // WTF? when I use the _ notation for the closure it doesn't print the last comma?
    s1.append("-")
    s1.append(x)
  }
  
  // 2) Use underscore
  // NOTE the '-' is not correctly added to the other elements
  val s2 = new StringBuilder
  l foreach {
    s2.append("-")
    s2.append(_)
    /*
     * Explanation: The real question is in which "context" the underscore is expanded to an
     * anonymous function. Here, "s2.append(_)" is expanded to "x => s2.append(x)". That is the
     * last line creates a value of the type String => Unit, which is returned since it is the
     * last value of the surrounding expression block.
     * 
     * This value of the block is then given to foreach. For each element of l it now just calls
     * append(element), but not the append("-"). Instead, this function is called only once, namely
     * during the evaluation of this expression block _before_ calling foreach.
     */
    // NOTE you can test this by uncommenting the following line: It makes the block return a String
//    "some string"
  }

  println("{x => ... append(x)} result: " + s1.toString)
  println("{... append(_)} result:      " + s2.toString)

}