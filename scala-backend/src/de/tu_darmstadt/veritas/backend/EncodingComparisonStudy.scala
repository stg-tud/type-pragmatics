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
  override def finalEncoding(m: Module)(implicit config: Configuration) = ToFof.toFofFile(m)
}
//fof with type guards
case object FofGuard extends Typing {
  override def finalEncoding(m: Module)(implicit config: Configuration) = ToFof.toFofFile(m)
}
//tff encoding
case object Tff extends Typing {
  override def finalEncoding(m: Module)(implicit config: Configuration) = ToTff.toTffFile(m)
}


case class AlternativeTyping(select: Configuration => Typing) extends Typing {
  override def finalEncoding(m: Module)(implicit config: Configuration) =
    select(config).finalEncoding(m)

}


object ConstructorTrans extends Alternative(selectConfig(FinalEncoding) {
  case FinalEncoding.BareFOF =>
    GenerateCtorAxioms
  case FinalEncoding.GuardedFOF =>
    SeqTrans(GenerateCtorAxioms, GenerateTypeGuards)
  case FinalEncoding.TFF =>
    Identity // TODO is this correct, or do we need GenerateCtorAxioms?
})

object BasicTrans extends SeqTrans(
  ResolveImports,
  VarToApp0,
  DesugarLemmas,
  ConstructorTrans,
  GenerateDiffAxiomsForConsts,
  FunctionEqToAxiomsSimple,
  TranslateTypingJudgmentToFunction,
  TranslateTypingJudgmentSimpleToFunction)

/**
 * determine which different problems are encoded ("Fragestellungen")
 */
object ProblemTrans extends Alternative(selectConfig(Problem) {
  case Problem.Consistency =>
    SplitModulesByGoal.setGoalFilter("")
    SeqTrans(SplitModulesByGoal, MoveDeclsToFront, SetupConsistencyCheck)
  case Problem.Proof =>
    SplitModulesByGoal.setGoalFilter("proof")
    SeqTrans(SplitModulesByGoal, MoveDeclsToFront)
  case Problem.Test =>
    SplitModulesByGoal.setGoalFilter("test")
    SeqTrans(SplitModulesByGoal, MoveDeclsToFront)
})

/**
 * determine whether subformulas in axioms/goals are inlined or named with an additional variable
 * (which adds an equation to the set of premises of axioms/goals)
 */
object VariableTrans extends Alternative(selectConfig(VariableEncoding) {
  // add InlineEverythingFP?
  case VariableEncoding.Unchanged =>
    Identity
  case VariableEncoding.NameEverything =>
    NameEverythingButMetaVars
  case VariableEncoding.InlineEverything =>
    InlineEverythingAndRemovePremsFP
  case VariableEncoding.NameParamsAndResults =>
    SeqTrans(NameFunctionResultsOnly, NameSubstituteFunctionDefParametersOnly)
})

object MainTrans extends SeqTrans(
  // desugar Veritas constructs
  BasicTrans,
  // determines whether and which inversion axioms are generated for functions/typing rules
  Optional(TotalFunctionInversionAxioms, ifConfig(InversionLemma, InversionLemma.On)), // ignored: InversionAll
  // variable inlining/extraction
  VariableTrans,
  // insert type guards for quantified metavariables
  Optional(InsertTypeGuardsForMetavars, ifConfig(FinalEncoding, FinalEncoding.GuardedFOF)),
  // determines whether logical optimizations take place prior to fof/tff encoding
  Optional(LogicalTermOptimization, ifConfig(LogicalSimplification, LogicalSimplification.On)),
  // select problem
  ProblemTrans)

object TypingTrans extends AlternativeTyping(selectConfig(FinalEncoding) {
  case FinalEncoding.BareFOF    => FofBare
  case FinalEncoding.GuardedFOF => FofGuard
  case FinalEncoding.TFF        => Tff
})

case class EncodingComparison(vm: VariabilityModel, module: Module) extends Iterable[(Configuration, Seq[PrettyPrintableFile])] {
  private var _lastConfig: Configuration = _
  def lastConfig = _lastConfig
  
  def iterator = vm.iterator.map { config =>
    _lastConfig = config
    val mods = MainTrans(Seq(module))(config)
    val files = mods.map(m => TypingTrans.finalEncoding(m)(config))
    (config, files)
  }
}



//  var encodingStrategies: Map[String, Seq[Module] => Seq[PrettyPrintableFile]] = TreeMap(
//    ("inconsistencies-partial-functions" ->
//      ((sm: Seq[Module]) => {
//        val transformedModules =
//          SetupConsistencyCheck(
//            MoveDeclsToFront(
//              SplitModulesByGoal(
//                LogicalTermOptimization(
//                  AllFunctionInversionAxioms(
//                    TranslateTypingJudgmentSimpleToFunction(
//                      TranslateTypingJudgmentToFunction(
//                        FunctionEqToAxiomsSimple(
//                          GenerateCtorAxioms(
//                            DesugarLemmas(
//                              VarToApp0(
//                                ReplaceImportsWithModuleDefs(ResolveImports(sm)))))))))))))
//        transformedModules map ToFof.toFofFile
//      })),
//    ("inconsistencies-wrong-constant-encoding" ->
//      ((sm: Seq[Module]) => {
//        val transformedModules =
//          SetupConsistencyCheck(
//            MoveDeclsToFront(
//              SplitModulesByGoal(
//                LogicalTermOptimization(
//                  AllFunctionInversionAxioms(
//                    TranslateTypingJudgmentSimpleToFunction(
//                      TranslateTypingJudgmentToFunction(
//                        FunctionEqToAxiomsSimple(
//                          GenerateCtorAxioms(
//                            DesugarLemmas(
//                              VarToApp0(
//                                ReplaceImportsWithModuleDefs(ResolveImports(sm)))))))))))))
//        transformedModules map ToFof.toFofFile
//      }))) ++ buildStrategies()
