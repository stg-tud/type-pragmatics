package de.tu_darmstadt.veritas.inputdsl

/**
 * Abstract class for any kind of reference to a specification construct
 */
class Ref[+S <: SpecConstruct](name: String, construct: S) {
  def getName() = name
  def getConstruct() = construct
}

