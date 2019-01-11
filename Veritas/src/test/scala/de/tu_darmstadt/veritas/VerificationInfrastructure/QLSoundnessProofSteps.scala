package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}


object QLSoundnessProofSteps {
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.QLDefsSpec._
  import de.tu_darmstadt.veritas.inputdsl.QLDefsTypeSystem._

  val fullQLspec: Module = Module("QLspec", Seq(),
    BasicTypes.defs ++ QLSyntax.defs ++ QLSemanticsData.defs ++ QLSemantics.defs ++ QLTypeSystem.defs)// ++ QLTypeSystemInv.defs)

  val QLProgress = goal(
    ((!'isValue('QC(~'am, ~'qm, ~'q))) &
      ('typeAM(~'am) === ~'atm) &
      ('typeQM(~'qm) === ~'qtm) &
      ('MC(~'atm, ~'qtm) |- ~'q :: 'MC(~'atm2, ~'qtm2))
    ).===>("QL-Progress")(
      exists(~'am0, ~'qm0, ~'q0) |
        'reduce(~'q, ~'am, ~'qm) === 'someQConf('QC(~'am0, ~'qm0, ~'q0)))
  )

  val ReduceExpProgress = lemma(
    ((!'expIsValue(~'exp)) &
    // ((~'exp != 'constant(~'av)) &
      ('typeAM(~'am) === ~'atm) &
      ('echeck(~'atm, ~'exp) === 'someAType(~'at))
      ).===>("ReduceExp-Progress")(
      exists(~'exp0) |
        'reduceExp(~'exp, ~'am) === 'someExp(~'exp0))
  )

  val LookupAnsMapProgress = lemma(
    (('typeAM(~'am) === ~'atm) &
      ('lookupATMap(~'qid, ~'atm) === 'someAType(~'at0))
    ).===>("LookupAnsMap-Progress")(
    exists(~'av0) | 'lookupAnsMap(~'qid, ~'am) === 'someAval(~'av0))
  )

  val GetExpProgress = lemma(
    ('isSomeExp(~'eOpt)
    ).===>("GetExp-Progress")(
      exists(~'exp0) | 'getExp(~'eOpt) === ~'exp0)
  )

  val LookupQMapProgress = lemma(
    (('typeQM(~'qm) === ~'qtm) &
    ('lookupATMap(~'qid, ~'qtm) === 'someAType(~'at))
  ).===>("LookupQMap-Progress")(
    exists(~'qid0, ~'l0, ~'t0) | 'lookupQMap(~'qid, ~'qm) === 'someQuestion(~'qid0, ~'l0, ~'t0))
  )
}
