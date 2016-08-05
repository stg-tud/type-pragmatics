package de.tu_darmstadt.veritas.inputdsl

/**
 * defining meta variables
 * only specified meta variables can be defined
 * similar to algebraic datatype definitions
 */
class MetaVarDefinition(nameref: Ref[SpecifiedMetaVar], alternativeShapes: Seq[SyntacticForm]) extends SpecConstruct(nameref.getName()) {
  
  def getConsNames() = alternativeShapes map (_.getName())
  
  //TODO improve this check, will not detect more complex cycles
  override def checkConsistency(): Boolean = 
    getConsNames().distinct == getConsNames &&
    alternativeShapes.exists(baseCase(_, Set(nameref)))
   
  
  private def baseCase(cons: SyntacticForm, lookedup: Set[Ref[MetaVar]]): Boolean = 
    cons.getArgs() contains { (a: Ref[MetaVar]) => !(lookedup contains a)}
  
}

/**
 * corresponds to constructor declaration
 */
class SyntacticForm(consName: String, args: Seq[Ref[MetaVar]]) extends SpecConstruct(consName) {
  def getArgs() = args
  
  override def checkConsistency(): Boolean = true
}