package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

import de.tu_darmstadt.veritas.backend.ast.{VeritasConstruct, VeritasFormula}

class VeritasSpecEnquirer extends SpecEnquirer[VeritasConstruct, VeritasFormula] {
  override def isRecursiveFunction(functioncall: VeritasConstruct) = ???

  override def isClosedADT(v: VeritasConstruct) = ???

  override def isForall(g: VeritasFormula) = ???

  override def isExists(g: VeritasFormula) = ???

  override def isImplication(g: VeritasFormula) = ???

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    *
    * @param quantifiedBody
    * @return
    */
  override def getQuantifiedBody(quantifiedBody: VeritasFormula) = ???
override def getArguments(functioncall: VeritasConstruct) = ???
override def getCases(v: VeritasConstruct) = ???
override def getRecArgsADT(c: VeritasConstruct) = ???
override def getUniversallyQuantifiedVars(g: VeritasFormula) = ???
override def getPremises(g: VeritasFormula) = ???
override def getConclusions(g: VeritasFormula) = ???
override def getVars(varblock: VeritasConstruct) = ???
override def getFormulae(formblock: VeritasFormula) = ???
override def getFormulaName(f: VeritasFormula) = ???
override def extractFunctionCalls(s: VeritasConstruct) = ???
override def extractFreeVariables(d: VeritasConstruct) = ???
override def consolidateFreeVariableNames[D <: VeritasConstruct](nd: D, refd: D) = ???
override def makeForall(vars: Seq[VeritasConstruct], body: VeritasFormula) = ???
override def makeForallQuantifyFreeVariables(body: VeritasFormula, fixed: Seq[VeritasConstruct]) = ???
override def makeImplication(prems: Seq[VeritasFormula], concs: Seq[VeritasFormula]) = ???
override def makeEquation(left: VeritasConstruct, right: VeritasConstruct) = ???

  override def makeNamedFormula(f: VeritasFormula, name: String) = ???
}
