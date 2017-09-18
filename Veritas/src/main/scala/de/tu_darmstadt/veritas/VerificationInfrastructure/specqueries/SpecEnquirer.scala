package de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries

/**
  * name convention for methods:
  * - extract... methods get any top-level Defs construct and recursively traverse the construct collecting wanted information
  * - get... methods already get a specific Defs construct (e.g. a forall construct) and return a structural part of that construct (e.g. the body of a forall)
  *
  * @tparam Defs Types for definitions, e.g. variables, ADTs, underspecified types, functions, including properties (axioms, lemmas, goals)
  * @tparam Formulae Types for formulae, e.g. axioms, lemmas, goals
  *
  */
trait SpecEnquirer[Defs, Formulae <: Defs] {

  //queries regarding the shape of a definition
  def isRecursiveFunction(functioncall: Defs): Boolean

  def isClosedADT(v: Defs): Boolean

  def isForall(g: Formulae): Boolean

  def isImplication(g: Formulae): Boolean

  /**
    * receive a formula that is universally or existentially quantified, return the body of the formula
    * @param quantifiedBody
    * @return
    */
  def getQuantifiedBody(quantifiedBody: Formulae): Formulae

  def getArguments(functioncall: Defs): Seq[Defs]

  // for a variable of type closed ADT, extract the different cases
  def getCases(v: Defs): Seq[Defs]

  //from an ADT case, extract the recursive arguments (may be empty if there are none)
  def getRecArgsADT(c: Defs): Seq[Defs]

  //expects a universally quantified formula, hands back a list of variables
  // (which we define as not being formulas by themselves - is that a good idea?)
  //for other formulae, returns the empty sequence
  def getUniversallyQuantifiedVars(g: Formulae): Seq[Defs]

  //expects a quantified formula (existentially or universally quantified)
  //returns sequence of quantified variables (top-level)
  //for other formulae, returns the empty sequence
  //TODO: is getQuantifiedVars needed/useful?
  //def getQuantifiedVars(g: Formulae): Seq[Defs]

  //expects an implication and returns the sequence of conjuncts from the premise
  // the conjuncts themselves are formulae
  //for other formulae, returns the given sequence (interpreted as implication with empty premises!)
  def getPremises(g: Formulae): Seq[Formulae]

  //expects an implication and returns the sequence of conjuncts from the conclusion
  // the conjuncts themselves are formulae
  //for other formulae, returns the empty sequence
  def getConclusions(g: Formulae): Seq[Formulae]

  //receives a block of variable declarations, returns single variable declarations
  def getVars(varblock: Defs): Seq[Defs]

  def getFormulae(formblock: Formulae): Seq[Formulae]

  //expects a construct with a named formula and extracts the formula's name
  def getFormulaName(f: Formulae): String

  //query methods for extracting information from specification/goals
  def extractFunctionCalls(s: Defs): Seq[Defs]

  //from a given definition or formula, extract all free variables
  def extractFreeVariables(d: Defs): Seq[Defs]

  //renames all variables in given definition nd so that there are no name clashes with free variables in refd
  // returns definition nd with renamed variables and the sequence of free variables in the the renamed nd
  def consolidateFreeVariableNames(nd: Defs, refd: Defs): (Defs, Seq[Defs])

  //constructor functions
  def makeForall(vars: Seq[Defs], body: Formulae): Formulae

  def makeImplication(prems: Seq[Formulae], concs: Seq[Formulae]): Formulae

  def makeEquation(left: Defs, right: Defs): Formulae

  //expects a list of individual variables, and groups them to a block ov variable/constant declarations
  def makeVarGroup(vars: Seq[Defs]): Defs

  def makeFormulaGroup(ffs: Seq[Formulae]): Formulae

  //expects an unnamed formula or a named one and attaches or overwrites the new name
  def makeNamedFormula(f: Formulae, name: String): Formulae

  def makeEmptyFormula(): Formulae



}
