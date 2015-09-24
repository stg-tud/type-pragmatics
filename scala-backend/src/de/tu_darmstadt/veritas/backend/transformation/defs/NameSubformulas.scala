package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames

/**
 * introduce variable names for subformulas for which checkConstruct holds
 *
 */
trait NameSubformulas extends ModuleTransformation {
  var freshNames = new FreshNames
  
  //collect the new meta variable names that are generated
  var generatedNames: Set[MetaVar] = Set()

  //collect the additional naming premises that are generated during traversal
  var additionalPremises: Seq[TypingRuleJudgment] = Seq()

  /**
   * override to control which constructs get named subformulas
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   */
  def checkConstruct(vc: VeritasConstruct): Boolean = true

  /**
   * gets direct parent of subformula for which a meta variable is to be generated
   * as parameter
   *
   * this information can be used to construct a certain name
   * default implementation just generates a fresh name with "VAR"
   *
   */
  def newMetaVar(parent: VeritasConstruct): MetaVar = MetaVar(freshNames.freshName("VAR"))

  private def addAdditionalPremise(mv: MetaVar, mexp: FunctionExpMeta): Unit =
    additionalPremises = additionalPremises :+ FunctionExpJudgment(FunctionExpEq(FunctionMeta(mv), mexp))

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //reset additionalPremises & freshNames for each new typing rule that is traversed
    additionalPremises = Seq()
    freshNames = new FreshNames
    generatedNames = Set()

    withSuper(super.transTypingRules(tr)) {
      case TypingRule(n, prems, conss) => {
        //the following call will collect named subformulas as premises
        //and modify variable additionalPremises
        val transformedPremises = trace(prems)(transTypingRuleJudgments(_))
        val transformedConclusions = trace(conss)(transTypingRuleJudgments(_))
        Seq(TypingRule(n, additionalPremises ++ transformedPremises,
          transformedConclusions))
      }
    }
  }

  //TODO: how to treat ForallJudgment/ExistsJudgment??
  //current implementation does not pay attention to that!!

  private def checkAndSubstituteMexp(mexp: FunctionExpMeta): FunctionExpMeta =
    //code below makes sure that no substitution is performed if the given mexp is already
    //one of the generated meta variables
    if (checkConstruct(mexp)) {
      mexp match {
        case FunctionMeta(m) =>
          if (!(generatedNames contains m)) {
            val newmeta = newMetaVar(path.head)
            generatedNames += newmeta
            addAdditionalPremise(newmeta, mexp)
            FunctionMeta(newmeta)
          } else mexp
        case _ => {
          val newmeta = newMetaVar(path.head)
          generatedNames += newmeta
          addAdditionalPremise(newmeta, mexp)
          FunctionMeta(newmeta)
        }
      }
    } else mexp

  private def checkAndSubstituteMexps(mexp: FunctionExpMeta): Seq[FunctionExpMeta] =
    if (checkConstruct(mexp)) {
      mexp match {
        case FunctionMeta(m) =>
          if (!(generatedNames contains m)) {
            val newmeta = newMetaVar(path.head)
            generatedNames += newmeta
            addAdditionalPremise(newmeta, mexp)
            Seq(FunctionMeta(newmeta))
          } else Seq(mexp)
        case _ => {
          val newmeta = newMetaVar(path.head)
          generatedNames += newmeta
          addAdditionalPremise(newmeta, mexp)
          Seq(FunctionMeta(newmeta))
        }
      }
    } else Seq(mexp)

  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f)) {
      case fmv @ FunctionMeta(mv) => checkAndSubstituteMexp(fmv)
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper(super.transFunctionExpMetas(f)) {
      case fmv @ FunctionMeta(mv) => checkAndSubstituteMexps(fmv)
    }

  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper(super.transFunctionExps(f)) {
      case fe @ FunctionExpEq(f1, f2)    => Seq(FunctionExpEq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2)))
      case fe @ FunctionExpNeq(f1, f2)   => Seq(FunctionExpNeq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2)))
      case fe @ FunctionExpIf(c, t, e)   => Seq(FunctionExpIf(checkAndSubstituteMexp(c), checkAndSubstituteMexp(t), checkAndSubstituteMexp(e)))
      case fe @ FunctionExpLet(n, e, i)  => Seq(FunctionExpLet(n, checkAndSubstituteMexp(e), checkAndSubstituteMexp(i)))
      case fe @ FunctionExpApp(fn, args) => Seq(FunctionExpApp(fn, args map checkAndSubstituteMexp))
    }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    //note: naming of subformulas can anyway only ever be applied within constructs 
    //that accept FunctionExpMeta as arguments
    //(since substitution potentially replaces formula with MetaVar!)
    withSuper(super.transFunctionExp(f)) {
      case fe @ FunctionExpEq(f1, f2)    => FunctionExpEq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2))
      case fe @ FunctionExpNeq(f1, f2)   => FunctionExpNeq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2))
      case fe @ FunctionExpIf(c, t, e)   => FunctionExpIf(checkAndSubstituteMexp(c), checkAndSubstituteMexp(t), checkAndSubstituteMexp(e))
      case fe @ FunctionExpLet(n, e, i)  => FunctionExpLet(n, checkAndSubstituteMexp(e), checkAndSubstituteMexp(i))
      case fe @ FunctionExpApp(fn, args) => FunctionExpApp(fn, args map checkAndSubstituteMexp)
    }

}

object NameEverything extends NameSubformulas

/**
 * excludes all meta variables from being named
 */
object NameEverythingButMetaVars extends NameSubformulas {
  override def checkConstruct(vc: VeritasConstruct): Boolean =
    vc match {
      case FunctionMeta(_) => false
      case MetaVar(_)      => false
      case _               => true
    }
}

/**
 * names for non-boolean function results only (RESULT-variable)
 */
object NameFunctionResultsOnly extends NameSubformulas {
  override def checkConstruct(vc: VeritasConstruct): Boolean = {
    //only rename right-hand side of single equation in conclusion
    val grandgrandparent = path(2)

    grandgrandparent match {
      case TypingRule(n, prems, Seq(FunctionExpJudgment(FunctionExpEq(l, r)))) if (r == vc) => true
      case _ => false
    }
  }
  
  override def newMetaVar(parent: VeritasConstruct): MetaVar = MetaVar("RESULT")

}




