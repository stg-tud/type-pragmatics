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

trait StudyValue {
  val valname: String
  def apply(m: Seq[Module]): Seq[Module] = m
}
trait StudyVariable {
  val studyvalues: List[StudyValue]
}

/**
 * determine the final encoding of module
 */
trait TypingValue extends StudyValue {
  val valname: String
  override def apply(m: Seq[Module]): Seq[Module] = m
  def finalEncoding(): Module => PrettyPrintableFile
}

//just fof, completely untyped
case object FofBare extends TypingValue {
  override val valname = "FofBare"
  override def finalEncoding(): Module => PrettyPrintableFile = ToFof.toFofFile
}
//fof with type guards (not yet implemented!)
case object FofGuard extends TypingValue {
  override val valname = "FofGuard"
  override def finalEncoding(): Module => PrettyPrintableFile = ???
}
//tff encoding
case object Tff extends TypingValue {
  override val valname = "Tff"
  override def finalEncoding(): Module => PrettyPrintableFile = ToTff.toTffFile
}

case class Typing(vals: List[TypingValue]) extends StudyVariable {
  override val studyvalues = vals
}

/**
 * determine whether subformulas in axioms/goals are inlined or named with an additional variable
 * (which adds an equation to the set of premises of axioms/goals)
 */
trait SubformNamingValue extends StudyValue
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

case class SubformNaming(vals: List[SubformNamingValue]) extends StudyVariable {
  override val studyvalues = vals
}

/**
 *  determines whether logical optimizations take place prior to fof/tff encoding
 */
trait LogicalOptValue extends StudyValue
//optimization takes place
case object Optimized extends LogicalOptValue {
  override val valname = "Optimized"
  override def apply(m: Seq[Module]): Seq[Module] = LogicalTermOptimization(m)
}
//no optimization (modules may include stupid formulas)
case object NotOptimized extends LogicalOptValue {
  override val valname = "NotOptimized"
}

case class LogicalOpt(vals: List[LogicalOptValue]) extends StudyVariable {
  override val studyvalues = vals
}

/**
 * determines whether outcome file is for a consistency check (with false goal) or for a proof
 */
trait ConsistencyValue extends StudyValue
//file is set up for consistency check (with false goal)
case object Consistency extends ConsistencyValue {
  override val valname = "Consistency"
  override def apply(m: Seq[Module]): Seq[Module] = SetupConsistencyCheck(m)
}
//file is not changed (goal left as is)
case object Proof extends ConsistencyValue {
  override val valname = "Proof"
}

case class ConsistencyCheck(vals: List[ConsistencyValue]) extends StudyVariable {
  override val studyvalues = vals
}

/**
 * determines whether and which inversion axioms are generated for functions/typing rules
 */
trait InversionValue extends StudyValue
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

case class Inversion(vals: List[InversionValue]) extends StudyVariable {
  override val studyvalues = vals
}

// StudyVariables below are currently not variables at all, but a standard part of the transformation pipeline 
// (should not be changed!)

case object BasicEncodings extends StudyVariable {
  case object Basics extends StudyValue {
    override val valname = "" //no name, because should not appear in filename
    override def apply(m: Seq[Module]): Seq[Module] =
      TranslateTypingJudgmentSimpleToFunction(
        TranslateTypingJudgmentToFunction(
          FunctionEqToAxiomsSimple(
            GenerateCtorAxioms(
              DesugarLemmas(
                VarToApp0(
                  JoinConstructors(
                    ReplaceImportsWithModuleDefs(ResolveImports(m)))))))))

  }

  override val studyvalues = List(Basics)
}

case object GoalSplitting extends StudyVariable {
  case object Splitting extends StudyValue {
    override val valname = "" //no name, because should not appear in filename
    override def apply(m: Seq[Module]): Seq[Module] =
      MoveDeclsToFront(
        SplitModulesByGoal(m))
  }

  override val studyvalues = List(Splitting)
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

  val studyConfiguration: List[StudyVariable] = List(
    ConsistencyCheck(List(Consistency, Proof)),
    GoalSplitting,
    LogicalOpt(List(Optimized, NotOptimized)),
    SubformNaming(List(NoNamingChange, NameEverything,
      InlineEverythingAndRemove, NameParamsResults)),
    Inversion(List(InversionTotal, NoInversion)),
    BasicEncodings)

  def buildStrategies(): TreeMap[String, Seq[Module] => Seq[PrettyPrintableFile]] = {

    def buildModuleTransformationsRec(restlist: List[StudyVariable]): List[(String, Seq[Module] => Seq[Module])] =
      restlist match {
        case Nil            => List() //should not happen
        case headvar :: Nil => for (currStrategy <- headvar.studyvalues) yield (currStrategy.valname -> ((mseq: Seq[Module]) => currStrategy(mseq)))
        case headvar :: restvars => for {
          currStrategy <- headvar.studyvalues
          (name, transformation) <- buildModuleTransformationsRec(restvars)
        } yield (name + "-" + currStrategy.valname ->
          ((mseq: Seq[Module]) => currStrategy(transformation(mseq))))
      }

    val transformationStrategies = buildModuleTransformationsRec(studyConfiguration)

    val stratlist: List[(String, Seq[Module] => Seq[PrettyPrintableFile])] = for {
      enc <- typeEncodings.studyvalues
      (name, transformation) <- transformationStrategies
    } yield (name + "-" + enc.valname ->
      ((mseq: Seq[Module]) => for (mod <- transformation(mseq)) yield enc.finalEncoding()(mod)))

    TreeMap(stratlist: _*)
  }

  var encodingStrategies: Map[String, Seq[Module] => Seq[PrettyPrintableFile]] = TreeMap(
    ("test" -> ((sm: Seq[Module]) => {
      val transformedModules =
        NameSubstituteFunctionDefParametersOnly(NameFunctionResultsOnly(
          TotalFunctionInversionAxioms(
            TranslateTypingJudgmentSimpleToFunction(
              TranslateTypingJudgmentToFunction(
                FunctionEqToAxiomsSimple(
                  GenerateCtorAxioms(
                    DesugarLemmas(
                      VarToApp0(
                        JoinConstructors(
                          ReplaceImportsWithModuleDefs(ResolveImports(sm))))))))))))
      transformedModules

    })))

  //      ("inconsistencies-partial-functions" ->
  //        ((sm: Seq[Module]) => {
  //          val transformedModules =
  //            SetupConsistencyCheck(
  //              MoveDeclsToFront(
  //                SplitModulesByGoal(
  //                  LogicalTermOptimization(
  //                    AllFunctionInversionAxioms(
  //                      TranslateTypingJudgmentSimpleToFunction(
  //                        TranslateTypingJudgmentToFunction(
  //                          FunctionEqToAxiomsSimple(
  //                            GenerateCtorAxioms(
  //                              DesugarLemmas(
  //                                VarToApp0(
  //                                  JoinConstructors(
  //                                    ReplaceImportsWithModuleDefs(ResolveImports(sm))))))))))))))
  //          transformedModules map ToFof.toFofFile
  //        })),
  //      ("inconsistencies-wrong-constant-encoding" ->
  //        ((sm: Seq[Module]) => {
  //          val transformedModules =
  //            SetupConsistencyCheck(
  //              MoveDeclsToFront(
  //                SplitModulesByGoal(
  //                  LogicalTermOptimization(
  //                    AllFunctionInversionAxioms(
  //                      TranslateTypingJudgmentSimpleToFunction(
  //                        TranslateTypingJudgmentToFunction(
  //                          FunctionEqToAxiomsSimple(
  //                            GenerateCtorAxioms(
  //                              JoinConstructors(
  //                                DesugarLemmas(
  //                                  VarToApp0(
  //                                    ReplaceImportsWithModuleDefs(ResolveImports(sm))))))))))))))
  //          transformedModules map ToFof.toFofFile
  //        }))) ++ buildStrategies()

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
      encodingStrategies -= encodingStrategies.head._1

}