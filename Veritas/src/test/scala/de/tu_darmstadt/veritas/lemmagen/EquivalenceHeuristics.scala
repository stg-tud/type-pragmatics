package de.tu_darmstadt.veritas.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

object EquivalenceHeuristics {
  //generate a top-down traversal starting from the type of a given VeritasConstruct, based on ModuleTransformation
  private class JudgmentTraverser extends ModuleTransformation with Serializable {

    //subclasses can use this variable to collect the special Veritas constructs that they want to extract
    var collected: Seq[VeritasConstruct] = Seq()

    def apply(vc: VeritasConstruct): Seq[VeritasConstruct] = {
      vc match {
        case tp: TypingRule => transTypingRules(tp)
        case trj: TypingRuleJudgment => transTypingRuleJudgments(trj)
        case mv: MetaVar => transMetaVars(mv)
        case fe: FunctionExp => transFunctionExps(fe)
        case fem: FunctionExpMeta => transFunctionExpMetas(fem)
        case sr: SortRef => transSortRefs(sr)
        case _ => sys.error(s"Given Veritas construct not suppported by VeritasConstructTraverser: $vc")
      }
    }
  }

  def replaceMetaVars(judgment: TypingRuleJudgment, to: MetaVar): VeritasConstruct = {
    val replacer = new JudgmentTraverser {
      override def transMetaVars(m: MetaVar): Seq[MetaVar] = {
        Seq(to)
      }

      override def transMetaVar(m: MetaVar): MetaVar = {
        to
      }
    }
    replacer(judgment).head
  }

  def mightBeEquivalent(t1: TypingRule, t2: TypingRule): Boolean = {
    // we do not care about the name
    // rename all meta variables to ``_``,
    // get premises and consequences as sets
    // this will probably not typecheck
    var bottom = MetaVar("_")
    var t1premises = t1.premises.map(replaceMetaVars(_, bottom)).toSet
    var t1consequences = t1.consequences.map(replaceMetaVars(_, bottom)).toSet
    var t2premises = t2.premises.map(replaceMetaVars(_, bottom)).toSet
    var t2consequences = t2.consequences.map(replaceMetaVars(_, bottom)).toSet
    if(t1premises != t2premises || t1consequences != t2consequences)
      return false
    true
  }
}
