package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.backend.util.FreshNames

object FreshVariables {
  def freshName(bindings: Map[MetaVar, SortRef], typ: SortRef): String = {
    // get upper-cased characters of type name
    val upperPortion = typ.name.filter(_.isUpper)
    val prefix = if (upperPortion.isEmpty) "v" else upperPortion.toLowerCase
    freshName(bindings, prefix)
  }

  def freshName(bindings: Map[MetaVar, SortRef], prefix: String): String = {
    val freshNames = new FreshNames(false)

    var newName = ""
    do {
      newName = freshNames.freshName(prefix)
    } while (bindings.keys.exists(_.name == newName)) // TODO: might need to check for constants?
    newName
  }

  def freshMetaVar(bindings: Map[MetaVar, SortRef], typ: SortRef): MetaVar = {
    val mv = MetaVar(freshName(bindings, typ))
    mv.typ = Some(TypeInference.Sort(typ.name))
    mv
  }

  def freshMetaVars(bindings: Map[MetaVar, SortRef], types: Seq[SortRef]): Seq[(MetaVar, SortRef)] = {
    var newBindings = bindings
    var metaVars = Seq[(MetaVar, SortRef)]()
    for(typ <- types) {
      val mv = freshMetaVar(newBindings, typ)
      metaVars = metaVars :+ (mv, typ)
      newBindings = newBindings + (mv -> typ)
    }
    metaVars
  }
}

class Lemma(val bindings: Map[MetaVar, SortRef], val rule: TypingRule) {
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bindingsOfType(typ: SortRef): Seq[MetaVar] = {
    bindings.collect { case (metaVar, metaVarType) if typ == metaVarType => metaVar }.toSeq
  }

  def bind(variable: MetaVar): Lemma = {
    if(bindings contains variable) {
      sys.error(s"Variable already bound: $variable")
    }
    new Lemma(
      bindings + (variable -> variable.sortType),
      rule)
  }

  def withPremise(premise: TypingRuleJudgment): Lemma =
    new Lemma(
      bindings,
      TypingRule(rule.name, premise +: rule.premises, rule.consequences)
    )
}

class LemmaBuilder(baseLemma: Lemma) {
  private var lemma = baseLemma

  def bindings = lemma.bindings
  def rule = lemma.rule
  def boundTypes = lemma.boundTypes
  def bindingsOfType(typ: SortRef) = lemma.bindingsOfType(typ)

  def addPremise(premise: TypingRuleJudgment): Unit = {
    lemma = lemma.withPremise(premise)
  }

  def bindType(typ: SortRef): MetaVar = {
    val newVariable = FreshVariables.freshMetaVar(lemma.bindings, typ)
    lemma = lemma.bind(newVariable)
    newVariable
  }

  def bindTypes(types: Seq[SortRef]): Seq[MetaVar] = {
    var variables = Seq[MetaVar]()
    for(typ <- types) {
      variables :+= bindType(typ)
    }
    variables
  }

  def build() = lemma
  def copy(): LemmaBuilder = new LemmaBuilder(build())
}