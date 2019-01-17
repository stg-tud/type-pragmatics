package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import de.tu_darmstadt.veritas.scalaspl.util.{AugmentedCallGraph, VeritasAugmentedCallGraph, VeritasAugmentedCallGraphBuilder}

/**
  * top-level strategy for automatically generating a proof graph for progress/preservation proofs from a ScalaSPL specification
  * assumes a ScalaSPL specification which is properly annotated and contains progress/preservation top-level theorem and all necessary auxiliary lemmas
  *
  * @param pathtoScalaSPLsource
  * @param pathToStore
  */
class VeritasProgressPreservationTopLevelStrategy(pathtoScalaSPLsource: String, override val pathToStore: String)
  extends InitializationStrategyXodus[VeritasConstruct, VeritasFormula] with ProgressPreservationTopLevelStrategy[VeritasConstruct, VeritasFormula, DataType, FunctionDef, TypingRule, FunctionEq, FunctionExp, FunctionExpMeta] {

  val sourcefile = new File(pathtoScalaSPLsource)
  override val spec: Module = new ScalaSPLTranslator().translate(sourcefile)
  override val goalname_extractor: VeritasFormula => String = ProofGraphUI.extractGoalOrLemmaName


  override def computeDomainSpecificKnowledge(): DomainSpecificKnowledge[DataType, FunctionDef, TypingRule] = {
    //collect domain-specific knowledge
    val builder = VeritasDomainSpecificKnowledgeBuilder()
    builder.build(sourcefile)
  }


  override def getGoalsFromFunName(fn: String): Set[VeritasFormula] = {
    //TODO: implement this function so that may work with different lemma selection strategies

    //this is currently implemented so that it only works with functions which have BOTH a progress and a preservation property attached (e.g. top-level reduce)
    val tr_progress: TypingRule = dsk.lookupByFunName(dsk.progressProperties, fn).head
    val tr_preservation: TypingRule = dsk.lookupByFunName(dsk.preservationProperties, fn).head
    Set(Goals(Seq(tr_progress), None), Goals(Seq(tr_preservation), None))
  }

  override def createACG(fn: String): AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] = {
    //look for top-level reduce function by name
    val fun: FunctionDef = dsk.lookupByFunName(dsk.dynamicFunctions, fn) match {
      case Some(fd) => fd
      case _ => sys.error(s"Could not locate a function marked @Dynamic with name $fn.")
    }

    //construct augmented call graph from reduce function on
    val acg = new VeritasAugmentedCallGraphBuilder(spec).translate(fun, computeDomainSpecificKnowledge())(VeritasAugmentedCallGraph(fun.signature.name))

    //for debugging: visualize ACG
    val acg_name = s"acg_$fn.png"
    acg.visualizeACG(new File(acg_name))

    acg
  }

  //print progress obligation in ScalaSPL (using ScalaSPL pretty printer)
  val prettyPrinter = new SimpleToScalaSPLSpecificationPrinter {
    override val printer: PrettyPrintWriter = new PrettyPrintWriter(new PrintWriter(System.out))
  }

  private def prettyPrintObligation(obl: GenObligation[VeritasConstruct, VeritasFormula]): Unit = {

    val tr: TypingRule = obl.goal match {
      case Goals((t@TypingRule(_, _, _)) :: _, _) => t
      case t@TypingRule(_, _, _) => t
      case _ => sys.error("Expected a typing rule here but got " + obl.goal.toPrettyString())
    }

    prettyPrinter.printTypingRule(tr)
    prettyPrinter.printer.flush()
    //prettyPrinter.printer.close()
  }

  def printGoalWithName(name: String): Unit = {
    val obl = g.findObligation(name) match {
      case Some(o) => o
      case None => try {
        PG_UI.getObligation(name)
      } catch {
        case _ : Throwable => sys.error("Did not find an obligation with name " + name + " in the proof graph.")
      }
    }

    prettyPrintObligation(obl)
  }

  def printAllObligations(): Unit = {
    for (o <- g.obligationDFS()) {
      println(o.problemName + ": ")
      prettyPrintObligation(o)
    }

    prettyPrinter.printer.close()
  }


}
