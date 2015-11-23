package de.tu_darmstadt.veritas.backend

import de.tu_darmstadt.veritas.backend.veritas._
import scala.collection.immutable.TreeMap
import de.tu_darmstadt.veritas.backend.transformation._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import de.tu_darmstadt.veritas.backend.transformation.imports._
import de.tu_darmstadt.veritas.backend.transformation.lowlevel._
import de.tu_darmstadt.veritas.backend.util.prettyprint._
import de.tu_darmstadt.veritas.backend.fof.FofFile

// to change the study parameters, manipulate vals typeEncodings or studyConfiguration in EncodingComparisonStudy below

trait ConcreteEncoding {
  val valname: String
  def apply(m: Seq[Module]): Seq[Module] = m
}
trait EncodingAlternative {
  val studyvalues: List[ConcreteEncoding]
}

/**
 * determine the final encoding of module
 */
trait ConcreteTyping extends ConcreteEncoding {
  def finalEncoding(): Module => PrettyPrintableFile
}

//just fof, completely untyped
case object FofBare extends ConcreteTyping {
  override val valname = "FofBare"
  override def finalEncoding(): Module => PrettyPrintableFile = ToFof.toFofFile
}
//fof with type guards (not yet implemented!)
case object FofGuard extends ConcreteTyping {
  override val valname = "FofGuard"
  override def finalEncoding(): Module => PrettyPrintableFile = ???
}
//tff encoding
case object Tff extends ConcreteTyping {
  override val valname = "Tff"
  override def finalEncoding(): Module => PrettyPrintableFile = ToTff.toTffFile
}

case class Typing(vals: List[ConcreteTyping]) extends EncodingAlternative {
  override val studyvalues = vals
}

/**
 * determine whether subformulas in axioms/goals are inlined or named with an additional variable
 * (which adds an equation to the set of premises of axioms/goals)
 */
trait SubformNamingValue extends ConcreteEncoding
//no intermediate variables, inline all named subformulas
//but keep inlined premises
case object InlineEverything extends SubformNamingValue {
  override val valname = "InlineEverything"
  override def apply(m: Seq[Module]): Seq[Module] = InlineEverythingFP(m)
}
//no intermediate variables, inline all named subformulas
//and remove the inlined premises completely
case object InlineEverythingAndRemove extends SubformNamingValue {
  override val valname = "InlineEverythingAndRemove"
  override def apply(m: Seq[Module]): Seq[Module] = InlineEverythingAndRemovePremsFP(m)
}
//create names for all subformulas in an axiom/goal and add to premises
case object NameEverything extends SubformNamingValue {
  override val valname = "NameEverything"
  override def apply(m: Seq[Module]): Seq[Module] = NameEverythingButMetaVars(m)
}
//name all parameters and results of function applications (similar to old Veritas translation)
case object NameParamsResults extends SubformNamingValue {
  override val valname = "NameParamsResults"
  override def apply(m: Seq[Module]): Seq[Module] = NameSubstituteFunctionDefParametersOnly(NameFunctionResultsOnly(m))
}

case object NoNamingChange extends SubformNamingValue {
  override val valname = "NoNamingChange"
}

case class SubformNaming(vals: List[SubformNamingValue]) extends EncodingAlternative {
  override val studyvalues = vals
}

/**
 *  determines whether logical optimizations take place prior to fof/tff encoding
 */
trait LogicalOptValue extends ConcreteEncoding
//optimization takes place
case object Optimized extends LogicalOptValue {
  override val valname = "Optimized"
  override def apply(m: Seq[Module]): Seq[Module] = LogicalTermOptimization(m)
}
//no optimization (modules may include stupid formulas)
case object NotOptimized extends LogicalOptValue {
  override val valname = "NotOptimized"
}

case class LogicalOpt(vals: List[LogicalOptValue]) extends EncodingAlternative {
  override val studyvalues = vals
}

/**
 * determine which different problems are encoded ("Fragestellungen")
 */
trait ConcreteProblem extends ConcreteEncoding
//all files are set up for consistency check (with false goal)
case object ConsistencyAll extends ConcreteProblem {
  override val valname = "Consistency"
  override def apply(m: Seq[Module]): Seq[Module] = {
    SplitModulesByGoal.setGoalFilter("")
    SetupConsistencyCheck(
      MoveDeclsToFront(
        SplitModulesByGoal(m)))
  }
}

//generate files for all goals whose name starts with "proof"
case object Proof extends ConcreteProblem {
  override val valname = "Proof"
  override def apply(m: Seq[Module]): Seq[Module] = {
    SplitModulesByGoal.setGoalFilter("proof")
     MoveDeclsToFront(
        SplitModulesByGoal(m))
  }
}

//generate files for all goals whose name starts with "test"
case object Test extends ConcreteProblem {
  override val valname = "Test"
  override def apply(m: Seq[Module]): Seq[Module] = {
    SplitModulesByGoal.setGoalFilter("test")
     MoveDeclsToFront(
        SplitModulesByGoal(m))
  }
}

case class Problems(vals: List[ConcreteProblem]) extends EncodingAlternative {
  override val studyvalues = vals
}

/**
 * determines whether and which inversion axioms are generated for functions/typing rules
 */
trait InversionValue extends ConcreteEncoding
//inversion axioms generated for all total functions
case object InversionTotal extends InversionValue {
  override val valname = "InversionTotal"
  override def apply(m: Seq[Module]): Seq[Module] = TotalFunctionInversionAxioms(m)
}
// no inversion axioms generated
case object NoInversion extends InversionValue {
  override val valname = "NoInversion"
}
//inversion axioms are generated for all functions (including partial ones - unsound!)
case object InversionAll extends InversionValue {
  override val valname = "InversionAll"
  override def apply(m: Seq[Module]): Seq[Module] = AllFunctionInversionAxioms(m)
}

case class Inversion(vals: List[InversionValue]) extends EncodingAlternative {
  override val studyvalues = vals
}

// StudyVariables below are currently not variables at all, but a standard part of the transformation pipeline 
// (should not be changed!)

case object BasicEncodings extends EncodingAlternative {
  case object Basics extends ConcreteEncoding {
    override val valname = "" //no name, because should not appear in filename
    override def apply(m: Seq[Module]): Seq[Module] =
      TranslateTypingJudgmentSimpleToFunction(
        TranslateTypingJudgmentToFunction(
          FunctionEqToAxiomsSimple(
            GenerateCtorAxioms(
              DesugarLemmas(
                VarToApp0(
                  ReplaceImportsWithModuleDefs(ResolveImports(m))))))))

  }

  override val studyvalues = List(Basics)
}


class EncodingComparisonStudy {

  /**
   * general setup for encoding/selection variants
   * - add or remove case objects from above from parameter list of study variables
   * (variables without arguments should not be touched)
   * - changing the order of the variables influences the order of the module transformations! (cannot be shuffled arbitrarily!!)
   * (top strategies in list are applied last to input modules; last step is always one from typeEncodings)
   */

  val typeEncodings = Typing(List(FofBare, Tff))
  
  val consideredProblems = Problems(List(ConsistencyAll, Proof, Test))

  val encodingAlternatives: List[EncodingAlternative] = List(
    LogicalOpt(List(Optimized, NotOptimized)),
    SubformNaming(List(NoNamingChange, NameEverything,
      InlineEverythingAndRemove, NameParamsResults)),
    Inversion(List(InversionTotal, NoInversion)),
    BasicEncodings)

  def buildStrategies(): TreeMap[String, Seq[Module] => Seq[PrettyPrintableFile]] = {

    def buildModuleTransformationsRec(restlist: List[EncodingAlternative]): List[(String, Seq[Module] => Seq[Module])] =
      restlist match {
        case Nil            => List() //should not happen
        case headvar :: Nil => for (currStrategy <- headvar.studyvalues) yield (currStrategy.valname -> ((mseq: Seq[Module]) => currStrategy(mseq)))
        case headvar :: restvars => for {
          currStrategy <- headvar.studyvalues
          (name, transformation) <- buildModuleTransformationsRec(restvars)
        } yield (name + "-" + currStrategy.valname ->
          ((mseq: Seq[Module]) => currStrategy(transformation(mseq))))
      }

    val transformationStrategies = buildModuleTransformationsRec(consideredProblems +: encodingAlternatives)

    val stratlist: List[(String, Seq[Module] => Seq[PrettyPrintableFile])] = for {
      enc <- typeEncodings.studyvalues
      (name, transformation) <- transformationStrategies
    } yield (name + "-" + enc.valname ->
      ((mseq: Seq[Module]) => for (mod <- transformation(mseq)) yield enc.finalEncoding()(mod)))

    TreeMap(stratlist: _*)
  }

  var encodingStrategies: Map[String, Seq[Module] => Seq[PrettyPrintableFile]] = TreeMap(
    ("inconsistencies-partial-functions" ->
      ((sm: Seq[Module]) => {
        val transformedModules =
          SetupConsistencyCheck(
            MoveDeclsToFront(
              SplitModulesByGoal(
                LogicalTermOptimization(
                  AllFunctionInversionAxioms(
                    TranslateTypingJudgmentSimpleToFunction(
                      TranslateTypingJudgmentToFunction(
                        FunctionEqToAxiomsSimple(
                          GenerateCtorAxioms(
                            DesugarLemmas(
                              VarToApp0(
                                ReplaceImportsWithModuleDefs(ResolveImports(sm)))))))))))))
        transformedModules map ToFof.toFofFile
      })),
    ("inconsistencies-wrong-constant-encoding" ->
      ((sm: Seq[Module]) => {
        val transformedModules =
          SetupConsistencyCheck(
            MoveDeclsToFront(
              SplitModulesByGoal(
                LogicalTermOptimization(
                  AllFunctionInversionAxioms(
                    TranslateTypingJudgmentSimpleToFunction(
                      TranslateTypingJudgmentToFunction(
                        FunctionEqToAxiomsSimple(
                          GenerateCtorAxioms(
                            DesugarLemmas(
                              VarToApp0(
                                ReplaceImportsWithModuleDefs(ResolveImports(sm)))))))))))))
        transformedModules map ToFof.toFofFile
      }))) ++ buildStrategies()

  val encodingnum = encodingStrategies.size

  def currEncoding(module: Module): (String, Seq[PrettyPrintableFile]) = {
    if (encodingStrategies.isEmpty)
      ("", Seq())
    else {
      val headstrat = encodingStrategies.head
      val stratname = headstrat._1
      val strat = headstrat._2
      (stratname, strat(Seq(module)))
    }
  }

  def moveToNextEncoding(): Unit =
    if (!encodingStrategies.isEmpty)
      encodingStrategies = encodingStrategies.tail

}