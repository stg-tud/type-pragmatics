package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification
import de.tu_darmstadt.veritas.newinputdsl.translator.{SPLTranslationError, SPLTranslator}
import de.tu_darmstadt.veritas.newinputdsl.util.ScalaMetaUtils


trait SyntaxDirectedTypeCheckerGenerator[Spec <: SPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Typ <: Spec#Type] extends TypeCheckerGenerator[Spec, Context, Expression, Typ] {

  import scala.meta._

  var typingRules: Seq[TypingRule] = _
  var adts: Seq[DataType] = _
  var specPath: String = _

  override def generate(sourceString: String): TypeChecker[Spec, Context, Expression, Typ] = {
    this.specPath = ScalaMetaUtils.getObjectPath(sourceString)
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
        val funExpTranslator = RegisteredTermFunctionExpressionTranslator()
        // because we get case classes passed we get a string representation
        val veritasContext = funExpTranslator.translateExp(getTerm(context.toString))
        val veritasExp = funExpTranslator.translateExp(getTerm(exp.toString))
        val veritasTyp = funExpTranslator.translateExp(getTerm(typ.toString))
        val typingJudgment = TypingJudgment(veritasContext, veritasExp, veritasTyp)
        // check that typingJudgement does not contain any metas
        if (!DoesNotContainMetaVars.check(typingJudgment)) {
          throw new IllegalArgumentException("The typing judgment contains metavariables")
        }
        // built tree
        val constraintBuilder = new ConstraintBuilder { }
        val tree = deriveTree(typingJudgment)(constraintBuilder)
        // built constrains
        // solve constrains
        // substitue solution
        // check if tree is valid
        if (tree.nonEmpty) {
          val constraints = constraintBuilder.constraints
          val solution = NonRecursiveConstraintSolver.solve(constraints)(specPath)
          // TODO need to check that constraintset has no contradiction?
          if (solution.nonEmpty) {
            val subster = MetaVarSubstitution(solution.get)
            val x = new MetaVarCollection {}
            constraints.foreach(x.transFunctionExpMeta)
            val freeMetaVars = x.metaVars
            val backsubstituion = constraints.map {subster.transFunctionExpMeta}
            // see if any equations still contain metavars
            // execute the other side of the eq where no meta is contained and on the other side is a meta var
            // => metavar = result of other side
            val constraintsConsistent = backsubstituion.forall { exp => ReflectionHelper.executeFunctionExp(exp)(specPath, Map()).asInstanceOf[Boolean]}
            if (constraintsConsistent)
              tree.get.substitue(solution.get).check(specPath)
            else false
          } else false
        } else false
      }

      override def typable(exp: Expression, typ: Typ): Boolean = {
        val funExpTranslator = RegisteredTermFunctionExpressionTranslator()
        // because we get case classes passed we get a string representation
        val veritasExp = funExpTranslator.translateExp(getTerm(exp.toString))
        val veritasTyp = funExpTranslator.translateExp(getTerm(typ.toString))
        val typingJudgment = TypingJudgmentSimple(veritasExp, veritasTyp)
        false
        // canBeBuilt(typingJudgment)
      }
    }
  }


  def deriveTree(ruleJudgment: TypingRuleJudgment)(implicit constraintBuilder: ConstraintBuilder): Option[DerivationTree] = {
    ruleJudgment match {
      case fexp: FunctionExpJudgment =>
        return Some(FunctionJudgmentNode(fexp))
      case _ =>
    }
    // TODO adept method in such a way that we can set tj with functionapps in type
    // and collect constraint
    val matchingRule = getMatchingTypingRule(ruleJudgment)
    matchingRule match {
      case Some(typingRule) =>
        val premsTrees = typingRule.premises.map { deriveTree }
          if (premsTrees.forall(_.nonEmpty))
            Some(TypingRuleJudgmentNode(ruleJudgment, premsTrees.flatten.toSet))
          else None
      case None => None
    }
  }

  def getMatchingTypingRule(trj: TypingRuleJudgment)(implicit constraintBuilder: ConstraintBuilder): Option[TypingRule] = {
    val matchingTypingRules = typingRules.filter { tr =>
      ExpressionStructureChecker.check(trj, tr.consequences.head)
    }
    if (matchingTypingRules.size > 1 )
      throw SPLTranslationError("Could find more than one matching typing rule. This type system is not syntax-directed.")
    else if (matchingTypingRules.nonEmpty)
      constraintBuilder.build(trj, matchingTypingRules.head)
    else None
  }
}
