package de.tu_darmstadt.veritas.backend

import de.tu_darmstadt.veritas.backend.veritas._
import scala.collection.immutable.TreeMap
import de.tu_darmstadt.veritas.backend.transformation._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import de.tu_darmstadt.veritas.backend.transformation.imports._
import de.tu_darmstadt.veritas.backend.transformation.lowlevel._
import de.tu_darmstadt.veritas.backend.util.prettyprint._
import de.tu_darmstadt.veritas.backend.fof.FofFile

class EncodingComparisonStudy {

  var encodingStrategies: Map[String, Seq[Module] => Seq[PrettyPrintableFile]] = TreeMap(
    //    ("test-import" ->
    //      (sm => {
    //        val transformedModules =
    //          TranslateTypingJudgmentToFunction(
    //            FunctionEqToAxiomsSimple(
    //              GenerateCtorAxioms(
    //                JoinConstructors(
    //                  DesugarLemmas(
    //                    VarToApp0(
    //                      ReplaceImportsWithModuleDefs(ResolveImports(sm))))))))
    //        transformedModules
    //      })) 
    ("test-complete" ->
      (sm => {
        val transformedModules = MoveDeclsToFront(
          SplitModulesByGoal(
            LogicalTermOptimization(
              AllFunctionInversionAxioms(
                TranslateTypingJudgmentSimpleToFunction(
                  TranslateTypingJudgmentToFunction(
                    FunctionEqToAxiomsSimple(
                      GenerateCtorAxioms(
                        JoinConstructors(
                          DesugarLemmas(
                            VarToApp0(
                              ReplaceImportsWithModuleDefs(ResolveImports(sm)))))))))))))
        transformedModules map ToFof.toFofFile
      })) //    ("test-fof" ->
      //      (sm => {
      //        val transformedModules = MoveDeclsToFront(OldFunctionEqTransformation(FunctionEqToAxiomsSimple(VarToApp0(sm))))
      //        transformedModules map ToFof.toFofFile
      //      })),
      //    ("test-tff" ->
      //      (sm => {
      //        val transformedModules = MoveDeclsToFront(OldFunctionEqTransformation(FunctionEqToAxiomsSimple(VarToApp0(sm))))
      //        transformedModules map ToTff.toTffFile
      //      }))
      )

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