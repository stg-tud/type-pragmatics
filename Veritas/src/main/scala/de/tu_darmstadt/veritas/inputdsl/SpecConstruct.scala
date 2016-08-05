package de.tu_darmstadt.veritas.inputdsl

/**
 * common superclass for any specification construct
 */
abstract class SpecConstruct(name: String) {
  require(checkConsistency)
  
  def getName() = name
  def getRef(): Ref[this.type] = new Ref(name, this)
  
  def checkConsistency(): Boolean

}