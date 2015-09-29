package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError

/*
 * trait for collecting equations that can be inlined into other formulas
 */
trait CollectInlineEquations extends ModuleTransformation {
  
  //collect equations chosen for inlining (left side replaced by right side)
  var chosenSubstitutions: Map[MetaVar, FunctionExpMeta] = Map()
  

  /**
   * override to control which equations can be chosen for being inlined
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   */
  def checkConstruct(vc: VeritasConstruct): Boolean = true

  override def apply(m: Seq[Module]): Seq[Module] = {
    //make sure that any mutable state is initialized upon application!
    chosenSubstitutions = Map()
    super.apply(m)
  }
  
  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //the traversal of the other constructs will collect named subformulas as premises
    //and modify variable additionalPremises
    //reset additionalPremises & freshNames for each new typing rule that is traversed
    chosenSubstitutions = Map()
    tr match {
      case t @ TypingRule(n, prems, conss) => super.transTypingRules(t)
    }
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    trj match {
      case f @ FunctionExpJudgment(eq @ FunctionExpEq(FunctionMeta(m), r)) => {
        if (checkConstruct(eq))
          chosenSubstitutions = chosenSubstitutions + (m -> r) 
          //will always contain the binding for the last instance of a meta variable m within a premise list
        FunctionExpJudgment(trace(eq)(transFunctionExp(_)))
      }
      case e @ ExistsJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        res
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        res
      }
      case t => super.transTypingRuleJudgment(t)
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    trj match {
      case f @ FunctionExpJudgment(eq @ FunctionExpEq(FunctionMeta(m), r)) => {
        if (checkConstruct(eq))
          chosenSubstitutions = chosenSubstitutions + (m -> r)
          //will always contain the binding for the last instance of a meta variable m within a premise list
        Seq(FunctionExpJudgment(trace(eq)(transFunctionExp(_))))
      }
      case e @ ExistsJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        Seq(res)
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        Seq(res)
      }
      case t => super.transTypingRuleJudgments(t)
    }

}

trait InlineSubformulas extends ModuleTransformation with CollectInlineEquations {

}