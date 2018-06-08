package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification
import de.tu_darmstadt.veritas.scalaspl.translator.{ScalaSPLTranslationError, ScalaSPLTranslator}
import de.tu_darmstadt.veritas.scalaspl.util.ScalaMetaUtils


trait SyntaxDirectedTypeCheckerGenerator[Spec <: ScalaSPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Typ <: Spec#Type] extends TypeCheckerGenerator[Spec, Context, Expression, Typ] {

  import scala.meta._

  private var typingRules: Seq[TypingRule] = _
  private var adts: Seq[DataType] = _
  private var ctorNames: Seq[String] = _
  private var specPath: String = _

  override def generate(sourceString: String): TypeChecker[Spec, Context, Expression, Typ] = {
    this.specPath = ScalaMetaUtils.getObjectPath(sourceString)
    new TypeChecker[Spec, Context, Expression, Typ] {
      val translator = new ScalaSPLTranslator()
      val module: Module = translator.translate(sourceString)
      // collect information needed based on the translated Veritas AST
      adts = module.defs.collect { case dt: DataType => dt }
      ctorNames = adts.flatMap { _.constrs.map(_.name) }
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
        val constraintBuilder = new ConstraintBuilder { }
        val tree = deriveTree(typingJudgment)(constraintBuilder)
        if (tree.nonEmpty) {
          val solution = solveConstraints(constraintBuilder.constraints)
          if (solution.nonEmpty) {
            val solutionTree = tree.get.substitue(solution.get)
            return solutionTree.check(specPath)
          }
        }
        false
      }

      override def typable(exp: Expression, typ: Typ): Boolean = {
        val funExpTranslator = RegisteredTermFunctionExpressionTranslator()
        // because we get case classes passed we get a string representation
        val veritasExp = funExpTranslator.translateExp(getTerm(exp.toString))
        val veritasTyp = funExpTranslator.translateExp(getTerm(typ.toString))
        val typingJudgment = TypingJudgmentSimple(veritasExp, veritasTyp)
        // check that typingJudgement does not contain any metas
        if (!DoesNotContainMetaVars.check(typingJudgment)) {
          throw new IllegalArgumentException("The typing judgment contains metavariables")
        }
        check(typingJudgment)
      }
    }
  }

  def solveConstraints(constraints: Set[FunctionExpMeta]): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    // solve constraint set
    val solution = NonRecursiveConstraintSolver(ctorNames).solve(constraints)(specPath)
    if (solution.nonEmpty) {
      // check solution and constraint set are consistent
      val metaVarsSubstituter = MetaVarSubstitution(solution.get)
      val backsubstituion = constraints.map { metaVarsSubstituter.transFunctionExpMeta }
      val constraintsConsistent = backsubstituion.forall { exp =>
        ReflectionHelper.executeFunctionExp(exp)(specPath, Map()).asInstanceOf[Boolean]
      }
      if (constraintsConsistent) solution
      else None
    } else None
  }

  def check(trj: TypingRuleJudgment): Boolean = {
    // built tree
    val constraintBuilder = new ConstraintBuilder { }
    val tree = deriveTree(trj)(constraintBuilder)
    if (tree.nonEmpty) {
      val constraints = constraintBuilder.constraints
      // solve constraint set
      val solution = NonRecursiveConstraintSolver(ctorNames).solve(constraints)(specPath)
      if (solution.nonEmpty) {
        // check solution and constraint set are consistent
        val metaVarsSubstituter = MetaVarSubstitution(solution.get)
        val backsubstituion = constraints.map {metaVarsSubstituter.transFunctionExpMeta}
        val constraintsConsistent = backsubstituion.forall { exp => ReflectionHelper.executeFunctionExp(exp)(specPath, Map()).asInstanceOf[Boolean]}
        if (constraintsConsistent) {
          // subsitute solution
          val solutionTree = tree.get.substitue(solution.get)
          // check tree
          return solutionTree.check(specPath)
        }
      }
    }
    false
  }

  def deriveTree(ruleJudgment: TypingRuleJudgment)(implicit constraintBuilder: ConstraintBuilder): Option[DerivationTree] = {
    ruleJudgment match {
      case fexp: FunctionExpJudgment => return Some(FunctionJudgmentNode(fexp))
      case _ =>
    }
    val matchingTypingRules = typingRules.filter { tr =>
      ExpressionStructureChecker.check(ruleJudgment, tr.consequences.head)
    }
    if (matchingTypingRules.size > 1 )
      throw ScalaSPLTranslationError("Could find more than one matching typing rule. This type system is not syntax-directed.")
    else if (matchingTypingRules.isEmpty) None
    else {
      val matchingRule = constraintBuilder.build(ruleJudgment, matchingTypingRules.head)
      matchingRule match {
        case Some(typingRule) =>
          val premsTrees = typingRule.premises.map {
            deriveTree
          }
          if (premsTrees.forall(_.nonEmpty))
            Some(TypingRuleJudgmentNode(ruleJudgment, premsTrees.flatten.toSet))
          else None
        case None => None
      }
    }
  }

  def getMatchingTypingRule(trj: TypingRuleJudgment)(implicit constraintBuilder: ConstraintBuilder): Option[TypingRule] = {
    val matchingTypingRules = typingRules.filter { tr =>
      ExpressionStructureChecker.check(trj, tr.consequences.head)
    }
    if (matchingTypingRules.size > 1 )
      throw ScalaSPLTranslationError("Could find more than one matching typing rule. This type system is not syntax-directed.")
    else if (matchingTypingRules.nonEmpty)
      constraintBuilder.build(trj, matchingTypingRules.head)
    else None
  }
}
