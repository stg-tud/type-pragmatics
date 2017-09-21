package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.FixedVar
import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct

class VeritasSpecEnquirer extends SpecEnquirer[VeritasConstruct, VeritasConstruct] {

  override def isRecursiveFunction(functioncall: VeritasConstruct): Boolean = ???

  override def isClosedADT(v: VeritasConstruct): Boolean = ???

  override def isForall(g: VeritasConstruct): Boolean = ???

  override def isExists(g: VeritasConstruct): Boolean = ???

  override def isImplication(g: VeritasConstruct): Boolean = ???

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    *
    * @param quantifiedBody
    * @return
    */
  override def getQuantifiedBody(quantifiedBody: VeritasConstruct) = ???

  override def getArguments(functioncall: VeritasConstruct) = ???

  override def getCases(v: VeritasConstruct) = ???

  override def getRecArgsADT(c: VeritasConstruct) = ???

  override def getUniversallyQuantifiedVars(g: VeritasConstruct) = ???

  override def getPremises(g: VeritasConstruct) = ???

  override def getConclusions(g: VeritasConstruct) = ???

  override def getVars(varblock: VeritasConstruct) = ???

  override def getFormulae(formblock: VeritasConstruct) = ???

  override def getFormulaName(f: VeritasConstruct) = ???

  override def extractFunctionCalls(s: VeritasConstruct) = ???

  override def extractFreeVariables(d: VeritasConstruct) = ???

  override def consolidateFreeVariableNames[D <: VeritasConstruct](nd: D, refd: D) = ???

  override def makeForall(vars: Seq[VeritasConstruct], body: VeritasConstruct) = ???

  override def makeForallQuantifyFreeVariables(body: VeritasConstruct, fixed: Seq[FixedVar[VeritasConstruct]]) = ???

  override def makeImplication(prems: Seq[VeritasConstruct], concs: Seq[VeritasConstruct]) = ???

  override def makeEquation(left: VeritasConstruct, right: VeritasConstruct) = ???

  override def makeNamedFormula(f: VeritasConstruct, name: String) = ???

}
