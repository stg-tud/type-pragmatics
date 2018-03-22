package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification
import de.tu_darmstadt.veritas.newinputdsl.translator.{FunctionExpressionTranslator, SPLTranslationError, SPLTranslator}
import de.tu_darmstadt.veritas.newinputdsl.util.ScalaMetaUtils

trait SyntaxDirectedTypeCheckerGenerator[Spec <: SPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Typ <: Spec#Type] extends TypeCheckerGenerator[Spec, Context, Expression, Typ] {

  import scala.meta._

  var typingRules: Seq[TypingRule] = _
  var adts: Seq[DataType] = _
  var spec: Spec = _

  // TODO somehow get the spec from string without passing the spec
  override def generate(spec: Spec, sourceString: String): TypeChecker[Spec, Context, Expression, Typ] = {
    this.spec = spec
    new TypeChecker[Spec, Context, Expression, Typ] {
      val translator = new SPLTranslator()
      val module: Module = translator.translate(sourceString)
      // collect information needed based on the translated Veritas AST
      adts = module.defs.collect { case dt: DataType => dt }
      // if we have no typing rules (axioms) we should fail because there is no type system designed
      typingRules = module.defs.collectFirst { case axs: Axioms => axs.axioms }.get

      val getTerm: String => Term = ScalaMetaUtils.getTerm

      override def typable(context: Context, exp: Expression, typ: Typ): Boolean = {
        // variable should be metavar free
        val funExpTranslator = FunctionExpressionTranslator(Seq())
        // because we get case classes passed we get a string representation
        val veritasContext = funExpTranslator.translateExp(getTerm(context.toString))
        val veritasExp = funExpTranslator.translateExp(getTerm(exp.toString))
        val veritasTyp = funExpTranslator.translateExp(getTerm(typ.toString))
        val typingJudgment = TypingJudgment(veritasContext, veritasExp, veritasTyp)
        canBeBuilt(typingJudgment)
      }

      override def typable(exp: Expression, typ: Typ): Boolean = {
        val funExpTranslator = FunctionExpressionTranslator(Seq())
        // because we get case classes passed we get a string representation
        val veritasExp = funExpTranslator.translateExp(getTerm(exp.toString))
        val veritasTyp = funExpTranslator.translateExp(getTerm(typ.toString))
        val typingJudgment = TypingJudgmentSimple(veritasExp, veritasTyp)
        canBeBuilt(typingJudgment)
      }
    }
  }

  def canBeBuilt(ruleJudgment: TypingRuleJudgment): Boolean = {
    getMatchingTypingRule(ruleJudgment) match {
      case Some(typingRule) =>
        val premsBuilt = typingRule.premises.map { canBeBuilt }
        premsBuilt.forall { res => res }
      case None =>
        // if no matching typing rule could be found and is an functionexp we will execute it
        // otherwise we fail
        ruleJudgment match {
          case fexp: FunctionExpJudgment =>
            checkFunctionExp(fexp.f)(spec).asInstanceOf[Boolean]
          case _ => false
        }
    }
  }

  def getMatchingTypingRule(trj: TypingRuleJudgment): Option[TypingRule] = {
    val matcher = new MetaVarMatcher {}
    val matchingTypingRules = typingRules.filter { tr =>
      matcher.matchingMetaVars(trj, tr.consequences.head).nonEmpty
    }
    if (matchingTypingRules.size > 1)
      throw SPLTranslationError("Could find more than one matching typing rule. This type system is not syntax-directed.")
    else if (matchingTypingRules.nonEmpty)
      rewriteTypingRule(trj, matchingTypingRules.head)
    else None
  }

  // TODO Somehow i need to reverse a function app inside a conc to match it to other rule judgment
  // typing rules can not have any function applications inside of typing judgments only ctor applications
  // otherwise we need to create an inverse for every function and not every function has an inverse
  def rewriteTypingRule(bottom: TypingRuleJudgment, tr: TypingRule): Option[TypingRule] = {
    // get referenced metavars in top and the matching constructs in bottom
    // rewrite tr by replacing metavars with matching constructs
    val matcher = new MetaVarMatcher {}
    val matchingVars = matcher.matchingMetaVars(bottom, tr.consequences.head)
    if (matchingVars.nonEmpty) {
      val substituter = MetaVarSubstitution(matchingVars.get)
      val rewrittenPrems = tr.premises.map { substituter.transTypingRuleJudgment }
      val rewrittenCons = tr.consequences.map { substituter.transTypingRuleJudgment }
      Some(TypingRule(tr.name, rewrittenPrems, rewrittenCons))
    } else None
  }

  def checkFunctionExp(f: FunctionExpMeta)(implicit spec: SPLSpecification): Any = f match {
    case app: FunctionExpApp =>
      ReflectionHelper.execute(spec, app.toString())
    case FunctionExpNot(inner) =>
      !checkFunctionExp(inner).asInstanceOf[Boolean]
    case FunctionExpAnd(l, r) =>
      val executedLeft = checkFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = checkFunctionExp(r).asInstanceOf[Boolean]
      executedLeft && executedRight
    case FunctionExpOr(l, r) =>
      val executedLeft = checkFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = checkFunctionExp(r).asInstanceOf[Boolean]
      executedLeft || executedRight
    case FunctionExpBiImpl(l, r) =>
      val executedLeft = checkFunctionExp(l).asInstanceOf[Boolean]
      val executedRight = checkFunctionExp(r).asInstanceOf[Boolean]
      (executedLeft && executedRight) || (!executedLeft && !executedRight)
    case FunctionExpEq(l, r) =>
      val executedLeft = checkFunctionExp(l)
      val executedRight = checkFunctionExp(r)
      executedLeft == executedRight
    case FunctionExpNeq(l, r) =>
      val executedLeft = checkFunctionExp(l)
      val executedRight = checkFunctionExp(r)
      executedLeft != executedRight
    case FunctionExpIf(cond, thn, els) =>
      val executedCond = checkFunctionExp(cond).asInstanceOf[Boolean]
      if (executedCond) checkFunctionExp(thn)
      else checkFunctionExp(els)
    // TODO let
    case FunctionExpTrue => true
    case FunctionExpFalse => false
    case FunctionMeta(MetaVar(name)) =>
      throw new IllegalArgumentException("Function expression should not contain any metavariables")
  }
}
