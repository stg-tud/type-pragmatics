package de.tu_darmstadt.veritas.backend

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.Configuration._
import scala.collection.immutable.TreeMap
import de.tu_darmstadt.veritas.backend.transformation._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import de.tu_darmstadt.veritas.backend.transformation.imports._
import de.tu_darmstadt.veritas.backend.transformation.lowlevel._
import de.tu_darmstadt.veritas.backend.util.prettyprint._
import de.tu_darmstadt.veritas.backend.fof.FofFile

// to change the study parameters, manipulate vals typeEncodings or studyConfiguration in EncodingComparisonStudy below

/**
 * determine the final encoding of module
 */
trait Typing {
  def finalEncoding(m: Module)(implicit config: Configuration): PrettyPrintableFile
}

//just fof, completely untyped
case object FofBare extends Typing {
  override val valname = "FofBare"
  override def finalEncoding(m: Module)(implicit config: Configuration) = ToFof.toFofFile(m)
}
//fof with type guards (not yet implemented!)
case object FofGuard extends Typing {
  override val valname = "FofGuard"
  override def finalEncoding(m: Module)(implicit config: Configuration) = ???
}
//tff encoding
case object Tff extends Typing {
  override val valname = "Tff"
  override def finalEncoding(m: Module)(implicit config: Configuration) = ToTff.toTffFile(m)
}

case class AlternativeTyping(select: Configuration => Typing) {
  override def finalEncoding(m: Module)(implicit config: Configuration) =
    select(config).finalEncoding(m)

}


// StudyVariables below are currently not variables at all, but a standard part of the transformation pipeline 
// (should not be changed!)

object BasicTrans extends SeqTrans(
  ResolveImports,
  ReplaceImportsWithModuleDefs,
  VarToApp0,
  DesugarLemmas,
  Optional(GenerateCtorAxioms, ifConfig(FinalEncoding, FinalEncoding.BareFOF)),
  FunctionEqToAxiomsSimple,
  TranslateTypingJudgmentToFunction,
  TranslateTypingJudgmentSimpleToFunction
)

/**
 * determine which different problems are encoded ("Fragestellungen")
 */
object ProblemTrans extends Alternative(selectConfig(Problem){
  case Problem.Consistency =>   
    SplitModulesByGoal.setGoalFilter("")
    SeqTrans(SetupConsistencyCheck, MoveDeclsToFront, SplitModulesByGoal)
  case Problem.Proof =>
    SplitModulesByGoal.setGoalFilter("proof")
    SeqTrans(MoveDeclsToFront, SplitModulesByGoal)
  case Problem.Test =>
    SplitModulesByGoal.setGoalFilter("test")
    SeqTrans(MoveDeclsToFront, SplitModulesByGoal)
})

/**
 * determine whether subformulas in axioms/goals are inlined or named with an additional variable
 * (which adds an equation to the set of premises of axioms/goals)
 */
object VariableTrans extends Alternative(selectConfig(VariableEncoding){
  // add InlineEverythingFP?
  case VariableEncoding.Unchanged =>
    Identity
  case VariableEncoding.NameEverything =>
    NameEverythingButMetaVars
  case VariableEncoding.InlineEverything =>
    InlineEverythingAndRemovePremsFP
  case VariableEncoding.NameParamsAndResults =>
    SeqTrans(NameSubstituteFunctionDefParametersOnly, NameFunctionResultsOnly)
})


object EncodingTrans extends SeqTrans(
    // determines whether logical optimizations take place prior to fof/tff encoding
    Optional(LogicalTermOptimization, ifConfig(LogicalSimplification, LogicalSimplification.On)),
    // variable inlining/extraction
    VariableTrans,    
    // determines whether and which inversion axioms are generated for functions/typing rules
    Optional(TotalFunctionInversionAxioms, ifConfig(InversionLemma, InversionLemma.On)), // ignored: InversionAll
    // desugar Veritas constructs
    BasicTrans
)

object TypingTrans extends AlternativeTyping(selectConfig(FinalEncoding){
  case FinalEncoding.BareFOF => FofBare
  case FinalEncoding.GuardedFOF => FofGuard
  case FinalEncoding.TFF => Tff
})

class EncodingComparisonStudy {

  /**
   * general setup for encoding/selection variants
   * - add or remove case objects from above from parameter list of study variables
   * (variables without arguments should not be touched)
   * - changing the order of the variables influences the order of the module transformations! (cannot be shuffled arbitrarily!!)
   * (top strategies in list are applied last to input modules; last step is always one from typeEncodings)
   */

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