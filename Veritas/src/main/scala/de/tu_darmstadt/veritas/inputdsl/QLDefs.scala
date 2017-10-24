package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._

object QLDefs {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._

  // BasicTypes.stl
  val YN = data('YN) of 'yes | 'no

  val and: Functions = function('and.>>('YN, 'YN) -> 'YN) where
    ('and ('yes, 'yes) := 'yes ) |
    ('and ('b1, 'b2) := 'no)

  val or: Functions = function('or.>>('YN, 'YN) -> 'YN) where
    ('or ('no, 'no) := 'no ) |
      ('or ('b1, 'b2) := 'yes)

  val not: Functions = function('not.>>('YN) -> 'YN) where
    ('not ('yes) := 'no) |
    ('not ('no) := 'yes)

  val nat = data('nat) of 'zero | 'succ ('nat)

  val pred: Functions = function('pred.>>('nat) -> 'nat) where
    ('pred ('zero) := 'zero) |
    ('pred ('succ('n)) := 'n)

  val gt: Functions = function('gt.>>('nat, 'nat) -> 'YN) where
    ('gt ('zero, 'n) := 'no) |
    ('gt ('succ ('n), 'zero) := 'yes) |
    ('gt ('succ ('n1), 'succ ('n2)) := ('gt ('n1, 'n2)))

  val lt: Functions = function('lt.>>('nat, 'nat) -> 'YN) where
    ('lt ('n, 'zero) := 'no) |
      ('lt ('zero, 'succ ('n)) := 'yes) |
      ('lt ('succ ('n1), 'succ ('n2)) := ('lt ('n1, 'n2)))

  val plus: Functions = function('plus.>>('nat, 'nat) -> 'nat) where
    ('plus ('n1, 'zero) := 'n1) |
    ('plus ('n1, 'succ ('n2)) := ('succ ('plus ('n1, 'n2))))

  val minus: Functions = function('minus.>>('nat, 'nat) -> 'nat) where
    ('minus ('n1, 'zero) := 'n1) |
    ('minus ('n1, 'succ ('n2)) := ('pred ('minus ('n1, 'n2))))

  val multiply: Functions = function('multiply.>>('nat, 'nat) -> 'nat) where
    ('multiply ('n1, 'zero) := 'zero) |
    ('multiply ('n1, 'succ ('n2)) := ('plus ('n1, 'multiply ('n1, 'n2))))

  val divide: Functions = function('divide.>>('nat, 'nat) -> 'nat) where
    ('divide ('n1, 'n2) :=
      (iff ('gt ('n1, 'n2) === 'yes)
        th 'succ ('divide ('minus ('n1, 'n2)), 'n2)
        els 'zero))

  val char = open data 'char

  val string = data('string) of 'sempty | 'scons('char, 'string)

  val BasicTypes = Module("BasicTypes", Seq(),
    Seq(YN, and, or, not, nat, pred, gt, lt, plus, minus, multiply, divide, char, string))

  //QLSyntax.stl
  val QID = open data 'QID

  val GID = open data 'GID

  val Label = open data 'Label

  val Aval = data('Aval) of 'B('YN) | 'Num('nat) | 'T('string)

  val OptAval = data('OptAval) of 'noAval | 'someAval('Aval)

  val isSomeAval: Functions = function('isSomeAval.>>('OptAval) -> 'Bool) where
    ('isSomeAval ('noAval) := false) |
    ('isSomeAval ('someAval ('t)) := true)

  val getAval = partial(function('getAval.>>('OptAval) -> 'Aval) where
    ('getAval ('someAval ('t)) := 't))

  val AType = data('AType) of 'YesNo | 'Number | 'Text

  val OptAType = data('OptAType) of 'noAType | 'someAType('AType)

  val isSomeAType = function('isSomeAType.>>('OptAType) -> 'Bool) where
    ('isSomeAType ('noAType) := false) |
    ('isSomeAType ('someAType ('t)) := true)

  val getAType = partial(function('getAType.>>('OptAType) -> 'AType) where
    ('getAType ('someAType ('t)) := 't))

  val typeOf = function('typeOf.>>('Aval) -> 'AType) where
    ('typeOf ('B ('yn)) := 'YesNo) |
    ('typeOf ('Num ('n)) := 'Number) |
    ('typeOf ('T ('s)) := 'Text)

  val ATList = data('ATList) of 'atempty | 'atcons ('AType, 'ATList)

  val append = function('append.>>('ATList, 'ATList) -> 'ATList) where
    ('append ('atempty, 'atl) := 'atl) |
      ('append ('atcons ('at, 'atlr), 'atl) := 'atcons ('at, 'append ('atlr, 'atl)))

  val BinOp = data('BinOp) of 'addop | 'subop | 'mulop | 'divop | 'eqop |
    'gtop | 'ltop | 'andop | 'orop

  val UnOp = data('UnOp) of 'notop

  val Exp = data('Exp) of 'constant('Aval) |
    'qvar('QID) |
    'binop('Exp, 'BinOp, 'Exp) |
    'unop('UnOp, 'Exp)

  val Entry = data('Entry) of
    'question('QID, 'Label, 'AType) |
    'value('QID, 'AType, 'Exp) |
    'defquestion('QID, 'Label, 'AType) |
    'ask('QID)

  val Questionnaire = data('Questionnaire) of
    'qemtpy |
    'qsingle('Entry) |
    'qseq('Questionnaire, 'Questionnaire) |
    'qcond('Exp, 'Questionnaire, 'Questionnaire) |
    'qgroup('GID, 'Questionnaire)

  val QLSyntax = Module("QLSyntax", Seq(Resolved(BasicTypes)),
    Seq(QID, GID, Label, Aval, OptAval, isSomeAval, getAval,
      AType, OptAType, isSomeAType, getAType, typeOf, ATList,
      append, BinOp, UnOp, Exp, Entry, Questionnaire)) )

  //QLSemanticsData.stl
  val AnsMap = data('AnsMap) of 'aempty | 'abind('QID, 'Aval, 'AnsMap)

  val lookupAnsMap = function('lookupAnsMap.>>('QID, 'AnsMap) -> 'OptAval) where
    ('lookupAnsMap ('qid1, 'abind ('qid2, 'av, 'AM)) :=
      (iff ('qid1 === 'qid2)
        th 'someAval('av)
        els 'lookupAnsMap ('qid1, 'AM)))

  val appendAnsMap = function('appendAnsMap.>>('AnsMap, 'AnsMap) -> 'AnsMap) where
    ('appendAnsMap ('aempty, 'am) := 'am) |
    ('appendAnsMap ('abind('qid, 'av, 'am), 'aml) := 'abind ('qid, 'av, 'appendAnsMap ('am, 'aml)))

  val QMap = data('QMap) of 'qmempty | 'qmbind('QID, 'Label, 'AType, 'QMap)

  val OptQuestion = data('OptQuestion) of 'noQuestion | 'someQuestion('QID, 'Label, 'AType)

  val isSomeQuestion = function('isSomeQuestion.>>('OptQuestion) -> 'Bool) where
    ('isSomeQuestion ('noQuestion) := false) |
      ('isSomeQuestion ('someQuestion ('id, 'l, 't)) := true)

  val getQuestionQID = partial(function('getQuestionQID.>>('OptQuestion) -> 'QID) where
    ('getQuestionQID ('someQuestion ('qid, 'l, 't)) := 'qid))

  val getQuestionLabel = partial(function('getQuestionLabel.>>('OptQuestion) -> 'Label) where
    ('getQuestionLabel ('someQuestion ('qid, 'l, 't)) := 'l))

  val getQuestionAType = partial(function('getQuestionAType.>>('OptQuestion) -> 'AType) where
    ('getQuestionAType ('someQuestion ('qid, 'l, 't)) := 't))

  val lookupQMap = function('lookupQMap.>>('QID, 'QMap) -> 'OptQuestion) where
    ('lookupQMap ('qid, 'qmempty) := 'noQuestion) |
    ('lookupQMap ('qid1, 'qmbind ('qid2, 'l, 't, 'QM)) :=
      (iff ('qid1 === 'qid2)
        th 'someQuestion ('qid1, 'l, 't)
        els 'lookupQMap ('qid1, 'QM)))

  val appendQMap = function('appendQMap.>>('QMap, 'QMap) -> 'QMap) where
    ('appendQMap ('qmempty, 'qm) := 'qm) |
    ('appendQMap ('qmbind ('qid, 'l, 't, 'qm1), 'qm2) := 'qmbind ('qid, 'l, 't, 'appendQMap ('qm1, 'qm2)))

  val QConf = data('QConf) of 'QC ('AnsMap, 'QMap, 'Questionnaire)

  val getAM = function('getAM.>>('QConf) -> 'AnsMap) where
    ('getAM ('QC ('am, 'qm, 'q)) := 'am)

  val getQM = function('getQM.>>('QConf) -> 'QMap) where
    ('getQM ('QC ('am, 'qm, 'q)) := 'qm)

  val getQuest = function('getQuest.>>('QConf) -> 'Questionnaire) where
    ('getQuest ('QC ('am, 'qm, 'q)) := 'q)

  val isValue = function('isValue.>>('QConf) -> 'Bool) where
    ('isValue ('QC ('am, 'qm, 'qempty)) := true) |
    ('isValue ('QC ('am, 'qm, 'q)) := false)

  val OptQConf = data('OptQConf) of 'noQConf | 'someQConf ('QConf)

  val isSomeQC = function('isSomeQC.>>('OptQConf) -> 'Bool) where
    ('isSomeQC ('noQConf) := false) |
    ('isSomeQC ('someQConf ('qc)) := true)

  val getQC = partial(function('getQC.>>('OptQConf) -> 'QConf) where
    ('getQC ('someQConf ('qc)) := 'qc))

  val qcappend = function('qcappend.>>('QConf, 'Questionnaire) -> 'QConf) where
    ('qcappend ('QC ('am, 'qm, 'qs1), 'qs2) := 'QC ('am, 'qm, 'qseq ('qs1, 'qs2)))

  val OptExp = data('OptExp) of 'noExp | 'someExp('Exp)

  val isSomeExp = function('isSomeExp.>>('OptExp) -> 'Bool) where
    ('isSomeExp ('noExp) := false) |
      ('isSomeExp ('someExp ('e)) := true)

  val getExp = partial(function('getExp.>>('OptExp) -> 'Exp) where
    ('getExp ('someExp ('e)) := 'e))

  val expIsValue = function('expIsValue.>>('Exp) -> 'Bool) where
    ('expIsValue ('constant ('av)) := true) |
    ('expIsValue ('e) := false)

  val getExpValue = partial(function('getExpValue.>>('Exp) -> 'Aval) where
    ('getExpValue ('constant ('av)) := 'av))

  val QLSemanticsData = Module("QLSemanticsData", Seq(Resolved(BasicTypes), Resolved(QLSyntax)),
    Seq(AnsMap, lookupAnsMap, appendAnsMap, QMap, OptQuestion, isSomeQuestion, getQuestionQID,
      getQuestionLabel, getQuestionAType, lookupQMap, appendQMap, QConf, getAM, getQM,
      getQuest, isValue, OptQConf, isSomeQC, getQC, qcappend, OptExp, isSomeExp, getExp,
      expIsValue, getExpValue))

  //QLSemantics.stl

  val askYesNo = function('askYesNo.>>('Label) -> 'YN)

  val askNumber = function('askNumber.>>('Label) -> 'nat)

  val askString = function('askString.>>('Label) -> 'string)

  val getAnswer = function('getAnswer.>>('Label, 'AType) -> 'Aval) where
    ('getAnswer ('l, 'YesNo) := 'B ('askYesNo ('l))) |
    ('getAnswer ('l, 'Number) := 'Num ('askNumber ('l))) |
    ('getAnswer ('l, 'Text) := 'T ('askString ('l)))

  val evalBinOp = function('evalBinOp.>>('BinOp, 'Aval, 'Aval) -> 'OptExp) where
    ('evalBinOp ('addop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('Num ('plus ('n1, 'n2))))) |
    ('evalBinOp ('subop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('Num ('minus ('n1, 'n2))))) |
    ('evalBinOp ('mulop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('Num ('multiply ('n1, 'n2))))) |
    ('evalBinOp ('divop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('Num ('divide ('n1, 'n2))))) |
    ('evalBinOp ('gtop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('B ('gt ('n1, 'n2))))) |
    ('evalBinOp ('ltop, 'Num ('n1), 'Num ('n2)) := 'someExp ('constant ('B ('lt ('n1, 'n2))))) |
    ('evalBinOp ('andop, 'B ('b1), 'B ('b2)) := 'someExp ('constant ('B ('and ('b1, 'b2))))) |
    ('evalBinOp ('orop, 'B ('b1), 'B ('b2)) := 'someExp ('constant ('B ('or ('b1, 'b2))))) |
    ('evalBinOp ('eqop, 'Num ('n1), 'Num ('n2)) :=
      (iff ('n1 === 'n2)
        th 'someExp ('constant ('B ('yes)))
        els 'someExp ('constant ('B ('no))))) |
    ('evalBinOp ('eqop, 'B ('n1), 'B ('n2)) :=
      (iff ('b1 === 'b2)
        th 'someExp ('constant ('B ('yes)))
        els 'someExp ('constant ('B ('no))))) |
    ('evalBinOp ('eqop, 'T ('t1), 'T ('t2)) :=
      (iff ('t1 === 't2)
        th 'someExp ('constant ('B ('yes)))
        els 'someExp ('constant ('B ('no))))) |
    ('evalBinOp ('op, 'a1, 'a2) := 'noExp)

  val evalUnOp = function('evalUnOp.>>('UnOp, 'Aval) -> 'OptExp) where
    ('evalUnOp ('notop, 'B ('b)) := 'someExp ('constant ('B ('not ('b))))) |
    ('evalUnOp ('op, 'a) := 'noExp)

  val reduceExp = function('reduceExp.>>('Exp, 'AnsMap) -> 'OptExp) where
    ('reduceExp ('constant ('av), 'ams) := 'noExp) |
    ('reduceExp ('qvar ('qid), 'am) :=
      ((let('avOpt) := 'lookupAnsMap ('qid, 'am)) in
        (iff ('isSomeAval ('avOpt))
          th 'someExp ('constant ('getAval ('avOpt)))
          els 'noExp))) |
    ('reduceExp ('binop ('e1, 'op, 'e2), 'am) :=
      (iff ('expIsValue ('e1) && 'expIsValue ('e2))
        th 'evalBinOp ('op, 'getExpValue ('e1), 'getExpValue ('e2))
        els ((let('eOpt1) := 'reduceExp ('e1, 'am)) in
          (iff ('isSomeExp ('eOpt1))
            th 'someExp ('binop ('getExp ('eOpt1), 'op, 'e2))
            els 'noExp)))) |
    ('reduceExp ('unop ('op, 'e), 'am) :=
      (iff ('expIsValue ('e))
        th 'evalUnOp ('op, 'getExpValue ('e))
        els ((let('eOpt) := 'reduceExp ('e, 'am)) in
          (iff ('isSomeExp ('eOpt))
            th 'someExp ('unop ('op, 'getExp ('eOpt)))
            els 'noExp))))

  val reduce = function('reduce.>>('QConf) -> 'OptQConf) where
    ('reduce ('QC ('am, 'qm, 'qemtpy)) := 'noQConf) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('question ('qid, 'l, 't)))) :=
      ((let('av) := 'getAnswer ('l, 't)) in
        'someQConf ('QC ('abind ('qid, 'av, 'am), 'qm, 'qemtpy)))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('value ('qid, 'l, 'e)))) :=
      (iff ('expIsValue ('e))
        th 'someQConf ('QC ('abind ('qid, 'getExpValue ('e), 'am), 'qm, 'qempty))
        els ((let('eOpt) := 'reduceExp ('e, 'am)) in
        (iff ('isSomeExp ('eOpt))
          th 'someQConf ('QC ('am, 'qm, 'qsingle ('value ('qid, 't, 'getExp ('eOpt)))))
          els 'noQConf)))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('defquestion ('qid, 'l, 't)))) :=
      'someQConf ('QC ('am, 'qmbind ('qid, 'l, 't, 'qm), 'qempty))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('ask ('qid)))) :=
      ((let('qOpt) := 'looupQMap ('qid, 'qm)) in
        (iff ('isSomeQuestion ('qOpt))
          th 'someQConf ('QC ('am, 'qm, 'qsingle ('question ('qid, 'getQuestionLabel ('qOpt), 'getQuestionAType ('qOpt)))))
          els 'noQConf))) |
    ('reduce ('QC ('am, 'qm, 'qseq ('qempty, 'qs2))) := 'someQConf ('QC ('am, 'qm, 'qs2))) |
    ('reduce ('QC ('am, 'qm, 'qseq ('qs1, 'qs2))) :=
      ((let('qcOpt) := 'reduce('QC ('am, 'qm, 'qs1))) in
        (iff ('isSomeQC ('qcOpt))
          th 'someQConf ('qcappend ('getQC ('qcOpt), 'qs2))
          els 'noQConf))) |
    ('reduce ('QC ('am, 'qm, 'qcond ('constant ('B ('yes)), 'qs1, 'qs2))) :=
      'someQConf ('QC ('am, 'qm, 'qs1))) |
    ('reduce ('QC ('am, 'qm, 'qcond ('constant ('B ('no)), 'qs1, 'qs2))) :=
      'someQConf ('QC ('am, 'qm, 'qs2))) |
    ('reduce ('QC ('am, 'qm, 'qcond ('e, 'qs1, 'qs2))) :=
      ((let('eOpt) := 'reduceExp ('e, 'am)) in
        (iff ('isSomeExp ('eOpt))
          th 'someQConf ('QC ('am , 'qm, 'qcond ('getExp ('eOpt), 'qs1, 'qs2)))
          els 'noQConf))) |
    ('reduce ('QC ('am, 'qm, 'qgroup ('gid, 'qs))) := 'someQConf ('QC ('am, 'qm, 'qs)))

    val QLSemantics = Module("QLSemantics", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData)),
      Seq(askYesNo, askNumber, askString, getAnswer, evalBinOp, evalUnOp, reduceExp, reduce))
}
