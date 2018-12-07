package de.tu_darmstadt.veritas.scalaspl.translator

import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.scalaspl.util.Reporter

import scala.meta._

class FunctionExpressionTranslator(metavars: Seq[String]) {

  def translateExpMeta(term: Term): FunctionExpMeta = term match {
    case Term.Name(name) if metavars.contains(name) => FunctionMeta(MetaVar(name))
    case _ => translateExp(term)
  }

  def translateExp(term: Term): FunctionExp = term match {
    case Term.If(cond, thn, els) => translateIf(cond, thn, els)
    case Term.Block(stats) => translateBlock(stats)
    case Lit.Boolean(true) => FunctionExpTrue
    case Lit.Boolean(false) => FunctionExpFalse
    case Term.Apply(name, args) => translateApply(name, args)
    case Term.ApplyUnary(name, expr) =>
      if (name.value == "!")
        FunctionExpNot(translateExp(expr))
      else Reporter().report(s"The unary operator ${name.value} is not supported", term.pos.startLine)
    case Term.ApplyInfix(lhs, name, Nil, Seq(rhs)) => translateApplyInfix(lhs, name, rhs)
    case Term.Name(name) => FunctionExpVar(name)
    case _ => throw new IllegalArgumentException("")
  }

  private def translateIf(cond: Term, thn: Term, els: Term): FunctionExp =
    FunctionExpIf(translateExp(cond), translateExpMeta(thn), translateExpMeta(els))

  private def translateApply(name: Term, args: Seq[Term]): FunctionExp = {
    val transArgs = args.map { translateExpMeta }
    FunctionExpApp(name.toString, transArgs)
  }

  private def translateBlock(stats: Seq[Stat]): FunctionExpLet = {
    val bindings = stats.init.map {
      case Defn.Val(Seq(), Seq(Pat.Var(name)), None, rhs) =>
        (name, translateExpMeta(rhs))
      case v => Reporter().report("Found another statement than a definition of a val in the middle of a block", v.pos.startLine)
    }
    val in = stats.last match {
      case expr: Term => translateExpMeta(expr)
      case _ => Reporter().report("Last term of block is not an expression", stats.last.pos.startLine)
    }
    var let = in
    bindings.reverse.foreach { case (name, expr) =>
      let = FunctionExpLet(name.value, expr, let)
    }
    let.asInstanceOf[FunctionExpLet]
  }

  // assume that we have left assoc operators (&& ||) in spoofax it was right but that seems counterintuitive
  private def translateApplyInfix(lhs: Term, name: Term.Name, rhs: Term): FunctionExp = name.value match {
    case "==" => FunctionExpEq(translateExpMeta(lhs), translateExpMeta(rhs))
    case "!=" => FunctionExpNeq(translateExpMeta(lhs), translateExpMeta(rhs))
    case "&&" => FunctionExpAnd(translateExp(lhs), translateExp(rhs))
    case "||" => FunctionExpOr(translateExp(lhs), translateExp(rhs))
    case "<==>" => FunctionExpBiImpl(translateExp(lhs), translateExp(rhs))
    case op => Reporter().report(s"Unsupported operator $op was used", lhs.pos.startLine)
  }
}
