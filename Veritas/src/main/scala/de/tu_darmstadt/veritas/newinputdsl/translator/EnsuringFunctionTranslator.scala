package de.tu_darmstadt.veritas.newinputdsl.translator

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpFalse, FunctionExpTrue}
import de.tu_darmstadt.veritas.newinputdsl.util.Reporter

import scala.collection.mutable.ListBuffer
import scala.meta._

trait EnsuringFunctionTranslator {
  def reporter: Reporter

  def translate(fn: Defn.Def): TypingRule = {
    if (fn.decltpe.isEmpty)
      reporter.report("The return type of a function has to be explicitly defined", fn.pos.startLine)
    if (fn.tparams.nonEmpty)
      reporter.report("A function definition does not allow type parameters", fn.pos.startLine)
    if (fn.paramss.size > 1)
      reporter.report("A function definition can only have one parameter list", fn.pos.startLine)
    val metaBindings = fn.paramss.headOption.getOrElse(Nil).map { _.name.value }
    fn.body match {
      case Term.ApplyInfix(lhs, name, Nil, Seq(rhs)) if name.value == "ensuring" =>
        val (conclusions, requireBlock) = translateEnsuringClauses(lhs, rhs, metaBindings)
        val premises = translateJudgmentBlock(requireBlock)(metaBindings)
        TypingRule(fn.name.value, premises, conclusions)
      case _ => reporter.report("Axioms/Lemmas and Goals need to have at least one ensuring clause", fn.pos.startLine)
    }
  }

  private def translateJudgmentBlock(body: Term)(implicit metaVars: Seq[String] = Seq()): Seq[TypingRuleJudgment] = body match {
    case Term.Block(inner) => inner.map { translateRequire(_)(metaVars) }
    case _ => reporter.report("")
  }

  private def translateRequire(stat: Stat)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = stat match {
    case Term.Apply(name, arg::Nil) if name.toString == "require" => translateTypingRule(arg)(metaVars)
    case _ => reporter.report("Inside Axioms/Lemmas/Goals only require statements can be used", stat.pos.startLine)
  }

  private def translateEnsuringClauses(lhs: Term, rhs: Term, metavars: Seq[String]): (Seq[TypingRuleJudgment], Term) = {
    val ensurings = ListBuffer[TypingRuleJudgment]()
    var requireBlock: Term = null
    def process(next: Term): Unit = next match {
      case Term.ApplyInfix(l, Term.Name("ensuring"), Nil, r::Nil) =>
        process(l)
        process(r)
      case Term.Block(_) => requireBlock = next
      case _ =>
        ensurings += translateTypingRule(next)(metavars)
    }
    process(lhs)
    process(rhs)
    (ensurings, requireBlock)
  }

  private def translateTypingRule(term: Term)(implicit metaVars: Seq[String] = Seq()): TypingRuleJudgment = {
    val funTranslator = FunctionExpressionTranslator(metaVars)
    term match {
      case Lit.Boolean(true) => FunctionExpJudgment(FunctionExpTrue)
      case Lit.Boolean(false) => FunctionExpJudgment(FunctionExpFalse)
      case Term.Apply(name, arg::Nil)  if name.toString == "forall" =>
        val (vars, body) = translateQuantifiedExpr(arg)(metaVars)
        ForallJudgment(vars, body)
      case Term.Apply(name, arg::Nil)  if name.toString == "exists" =>
        val (vars, body) = translateQuantifiedExpr(arg)(metaVars)
        ExistsJudgment(vars, body)
      case Term.ApplyUnary(name, _) if name.value == "!" => FunctionExpJudgment(funTranslator.translateExp(term))
      case Term.Apply(_, _) => FunctionExpJudgment(funTranslator.translateExp(term))
      case Term.ApplyInfix(lhs, name, Nil, arg::Nil) =>
        name.value match {
          case "::" => TypingJudgmentSimple(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(arg))
          case "|-" => arg match {
            case Term.ApplyInfix(inner, Term.Name("::"), Nil, rhs::Nil) =>
              TypingJudgment(funTranslator.translateExpMeta(lhs), funTranslator.translateExpMeta(inner), funTranslator.translateExpMeta(rhs))
            case _ => reporter.report("Invalid operator in typing statement", term.pos.startLine)
          }
          case "||" => translateOrEnsuring(lhs, arg, metaVars)
          case _ =>
            FunctionExpJudgment(funTranslator.translateExp(term))
        }
    }
  }

  private def translateQuantifiedExpr(term: Term)(implicit metavars: Seq[String] = Seq()): (Seq[MetaVar], Seq[TypingRuleJudgment]) = term match {
    case Term.Function(params, body) =>
      val quantifiedvars = params.map { p => MetaVar(p.name.value) }
      val quantifiednames = quantifiedvars.map {_.name}
      (quantifiedvars, translateTypingRuleJudgementSequence(body)(quantifiednames ++ metavars))
  }

  private def translateTypingRuleJudgementSequence(term: Term)(implicit metavars: Seq[String] = Seq()): Seq[TypingRuleJudgment] = {
    term match {
      case Term.ApplyInfix(lhs, Term.Name("&"), Nil, rhs::Nil) =>
        translateTypingRuleJudgementSequence(lhs)(metavars) :+ translateTypingRule(rhs)(metavars)
      case _ => Seq(translateTypingRule(term)(metavars))
    }
  }

  private def translateOrEnsuring(lhs: Term, rhs: Term, metavars: Seq[String]): OrJudgment = {
    val cases = ListBuffer[Seq[TypingRuleJudgment]]()
    def process(term: Term): Unit = term match {
      case Term.ApplyInfix(l, Term.Name("||"), Nil, r::Nil) =>
        process(l)
        process(r)
      case _ =>
        cases += translateTypingRuleJudgementSequence(term)(metavars)
    }
    process(lhs)
    process(rhs)
    OrJudgment(cases)
  }
}

object EnsuringFunctionTranslator {
  def apply(r: Reporter): EnsuringFunctionTranslator = {
    new EnsuringFunctionTranslator {
      override val reporter: Reporter = r
    }
  }
}
