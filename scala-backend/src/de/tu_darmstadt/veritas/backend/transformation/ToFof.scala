package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

// TODO refactor and do proper modular subdivisions
object ToFof {
  def toFofFiles(veritasModule: Module): Seq[FofFile] = {
    val formulas = toFofFormulas(veritasModule)

    // divide into conjectures and everything else (TODO only axioms?)
    val isConjectureMap = formulas groupBy (_.role == Conjecture)
    val conjectures = isConjectureMap.getOrElse(true, Nil)
    val notConjectures = isConjectureMap.getOrElse(false, Nil)

    // create one file per conjecture
    for (conjectureFormula <- conjectures) yield {
      val inputModule = veritasModule.name.split('.').last
      FofFile(inputModule + "-" + conjectureFormula.name + ".fof", notConjectures :+ conjectureFormula)
    }
  }

  private def toFofFormulas(veritasModule: Module): Seq[FofAnnotated] = {
    // TODO put this in an optimization pass
    // collect all goals and axioms into one list each (such that they are not scattered over different ModuleDefs)
    val collectedGoals = veritasModule.body.collect({ case Goals(goals, _) => goals }).flatten
    val collectedAxioms = veritasModule.body.collect({ case Axioms(axioms) => axioms }).flatten

    val goals = collectedGoals map (ToFof.toFofAnnotated(_, Conjecture))
    val axioms = collectedAxioms map (ToFof.toFofAnnotated(_, Axiom))

    goals ++ axioms
  }

  // TODO idiomatic Scala way for free functions
  private def toFofAnnotated(rule: TypingRule, typ: FormulaRole): FofAnnotated = {
    val usedVariableNames = (rule.premises ++ rule.consequences).flatMap(CollectVariables.allVariables).distinct
    val quantifiedVariables = usedVariableNames map (UntypedVariable(_))

    // premises are ANDed, consequences ORed
    // TODO put this in an optimization pass: simplify And(oneFormula) => oneFormula
    val premiseFormulas: FofUnitary = rule.premises match {
      case Seq()              => True
      case Seq(premise)       => ToFof.toFof(premise)
      case premises @ Seq(_*) => Parenthesized(And(premises map ToFof.toFof))
    }
    val consequenceFormulas: FofUnitary = rule.consequences match {
      // TODO goal without consequences ever useful?
      case Seq()                  => throw BackendError("Goal without consequences (list of consequences is empty)")
      case Seq(consequence)       => ToFof.toFof(consequence)
      case consequences @ Seq(_*) => Parenthesized(Or(consequences map ToFof.toFof))
    }

    FofAnnotated(rule.name, typ, ForAll(quantifiedVariables, Parenthesized(Impl(premiseFormulas, consequenceFormulas))))
  }

  // TODO make these methods more general
  private def toFof(rule: TypingRuleJudgment): FofUnitary = rule match {
    case TypingJudgment(f1, f2, f3) => Appl(UntypedFunSymbol("tcheck"), toFof(f1), toFof(f2), toFof(f3))
    case TypingJudgmentSimple(f1, f2) => Appl(UntypedFunSymbol("tchecksimple"), toFof(f1), toFof(f2))
    case FunctionExpJudgment(FunctionExpEq(f1, f2)) => Eq(toFof(f1), toFof(f2))
    case _ => throw BackendError("toFof(TypingRuleJudgement)")
  }

  private def toFof(exp: FunctionExpMeta): Term = exp match {
    case FunctionMeta(MetaVar(metavar)) => UntypedVariable(metavar)
    // FIXME how to ensure that "FunctionExp" will always only give Terms, not new Formulas?
    // case FunctionExpEq(f1, f2) => Variable("TODO eq")
    case _                              => throw BackendError("toFof(FunctionExp)")
  }
}

private object CollectVariables {
  // FIXME scala, which is more idiomatic?
  // a) implement method on trait with case class match
  // b) abstract method on trait, implement for each subclass
  // or c) put this method in toTptp pass, not here

  // TODO change from allVariables to freeVariables -> "does also do a typecheck"
  // FIXME Contract: may contain duplicates!
  def allVariables(rule: TypingRuleJudgment): Seq[String] = rule match {
    case TypingJudgment(f1, f2, f3)   => allVariables(f1) ++ allVariables(f2) ++ allVariables(f3)
    case TypingJudgmentSimple(f1, f2) => allVariables(f1) ++ allVariables(f2)
    case FunctionExpJudgment(f)       => allVariables(f)
    case _                            => List()
  }

  // FIXME String for variables is not typesafe...
  def allVariables(exp: FunctionExpMeta): Seq[String] = exp match {
    case FunctionMeta(MetaVar(metavar))     => List(metavar)
    case FunctionExpNot(f)                  => allVariables(f)
    case FunctionExpEq(f1, f2)              => allVariables(f1) ++ allVariables(f2)
    case FunctionExpNeq(f1, f2)             => allVariables(f1) ++ allVariables(f2)
    case FunctionExpOr(f1, f2)              => allVariables(f1) ++ allVariables(f2)
    case FunctionExpBiImpl(f1, f2)          => allVariables(f1) ++ allVariables(f2)
    case FunctionExpAnd(f1, f2)             => allVariables(f1) ++ allVariables(f2)
    case FunctionExpIf(c, f1, f2)           => allVariables(c) ++ allVariables(f1) ++ allVariables(f2)
    case FunctionExpLet(_, f1, f2)          => allVariables(f1) ++ allVariables(f2)
    case FunctionExpApp(_, args @ _*)       => args.flatMap(allVariables)
    case FunctionExpTrue | FunctionExpFalse => Nil
    // FIXME how is FunctionExpVar to handle?
    case _: FunctionExpVar                  => Nil
  }
}
