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
        th 'succ ('divide ('minus ('n1, 'n2), 'n2))
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
    'qempty |
    'qsingle('Entry) |
    'qseq('Questionnaire, 'Questionnaire) |
    'qcond('Exp, 'Questionnaire, 'Questionnaire) |
    'qgroup('GID, 'Questionnaire)

  val QLSyntax = Module("QLSyntax", Seq(Resolved(BasicTypes)),
    Seq(QID, GID, Label, Aval, OptAval, isSomeAval, getAval,
      AType, OptAType, isSomeAType, getAType, typeOf, ATList,
      append, BinOp, UnOp, Exp, Entry, Questionnaire))

  //QLSemanticsData.stl
  val AnsMap = data('AnsMap) of 'aempty | 'abind('QID, 'Aval, 'AnsMap)

  val lookupAnsMap = function('lookupAnsMap.>>('QID, 'AnsMap) -> 'OptAval) where
    ('lookupAnsMap ('qid, 'aempty) := 'noAval) |
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
    ('evalBinOp ('eqop, 'B ('b1), 'B ('b2)) :=
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
            els ((let('eOpt2) := 'reduceExp ('e2, 'am)) in
              (iff ('isSomeExp ('eOpt2))
                th 'someExp ('binop ('e1, 'op, 'getExp ('eOpt2)))
                els 'noExp)))))) |
    ('reduceExp ('unop ('op, 'e), 'am) :=
      (iff ('expIsValue ('e))
        th 'evalUnOp ('op, 'getExpValue ('e))
        els ((let('eOpt) := 'reduceExp ('e, 'am)) in
          (iff ('isSomeExp ('eOpt))
            th 'someExp ('unop ('op, 'getExp ('eOpt)))
            els 'noExp))))

  val reduce = function('reduce.>>('QConf) -> 'OptQConf) where
    ('reduce ('QC ('am, 'qm, 'qempty)) := 'noQConf) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('question ('qid, 'l, 't)))) :=
      ((let('av) := 'getAnswer ('l, 't)) in
        'someQConf ('QC ('abind ('qid, 'av, 'am), 'qm, 'qempty)))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('value ('qid, 't, 'e)))) :=
      (iff ('expIsValue ('e))
        th 'someQConf ('QC ('abind ('qid, 'getExpValue ('e), 'am), 'qm, 'qempty))
        els ((let('eOpt) := 'reduceExp ('e, 'am)) in
        (iff ('isSomeExp ('eOpt))
          th 'someQConf ('QC ('am, 'qm, 'qsingle ('value ('qid, 't, 'getExp ('eOpt)))))
          els 'noQConf)))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('defquestion ('qid, 'l, 't)))) :=
      'someQConf ('QC ('am, 'qmbind ('qid, 'l, 't, 'qm), 'qempty))) |
    ('reduce ('QC ('am, 'qm, 'qsingle ('ask ('qid)))) :=
      ((let('qOpt) := 'lookupQMap ('qid, 'qm)) in
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

  //QLTypeSystem.stl

  val ATMap = data('ATMap) of 'atmempty | 'atmbind ('QID, 'AType, 'ATMap)

  val lookupATMap = function('lookupATMap.>>('QID, 'ATMap) -> 'OptAType) where
    ('lookupATMap ('qid, 'atmempty) := 'noAType) |
    ('lookupATMap ('qid1, 'atmbind ('qid2, 'at, 'tm)) :=
      (iff ('qid1 === 'qid2)
        th 'someAType ('at)
        els 'lookupATMap ('qid1, 'tm)))

  val appendATMap = function('appendATMap.>>('ATMap, 'ATMap) -> 'ATMap) where
    ('appendATMap ('atmempty, 'atm) := 'atm) |
    ('appendATMap ('atmbind ('qid, 'at, 'atm), 'atml) := 'atmbind ('qid, 'at, 'appendATMap ('atm, 'atml)))

  val MapConf = data('MapConf) of 'MC ('ATMap, 'ATMap)

  val intersectATM = function('intersectATM.>>('ATMap, 'ATMap) -> 'ATMap) where
    ('intersectATM ('atmempty, 'atm2) := 'atmempty) |
    ('intersectATM ('atm1, 'atmempty) := 'atmempty) |
    ('intersectATM ('atmbind ('qid, 'at, 'atm1), 'atm2) :=
      ((let('atm1atm2) := 'intersectATM ('atm1, 'atm2)) in
        ((let('lAT) := 'lookupATMap ('qid, 'atm2)) in
          (iff ('isSomeAType ('lAT) && ('getAType ('lAT) === 'at))
            th 'atmbind ('qid, 'at, 'atm1atm2)
            els 'atm1atm2))))

  val OptMapConf = data('OptMapConf) of 'noMapConf | 'someMapConf ('MapConf)

  val isSomeMapConf = function('isSomeMapConf.>>('OptMapConf) -> 'Bool) where
    ('isSomeMapConf ('noMapConf) := false) |
    ('isSomeMapConf ('someMapConf ('mc)) := true)

  val getMapConf = partial(function('getMapConf.>>('OptMapConf) -> 'MapConf) where
    ('getMapConf ('someMapConf ('mc)) := 'mc))

  val typeAM = function('typeAM.>>('AnsMap) -> 'ATMap) where
    ('typeAM ('aempty) := 'atmempty) |
    ('typeAM ('abind ('qid, 'av, 'am)) := 'atmbind ('qid, 'typeOf ('av), 'typeAM ('am)))

  val typeQM = function('typeQM.>>('QMap) -> 'ATMap) where
    ('typeQM ('qmempty) := 'atmempty) |
    ('typeQM ('qmbind ('qid, 'l, 'at, 'qm)) := 'atmbind ('qid, 'at, 'typeQM ('qm)))

  val checkBinOp = function('checkBinOp.>>('BinOp, 'AType, 'AType) -> 'OptAType) where
    ('checkBinOp ('addop, 'Number, 'Number) := 'someAType ('Number)) |
    ('checkBinOp ('subop, 'Number, 'Number) := 'someAType ('Number)) |
    ('checkBinOp ('mulop, 'Number, 'Number) := 'someAType ('Number)) |
    ('checkBinOp ('divop, 'Number, 'Number) := 'someAType ('Number)) |
    ('checkBinOp ('gtop, 'Number, 'Number) := 'someAType ('YesNo)) |
    ('checkBinOp ('ltop, 'Number, 'Number) := 'someAType ('YesNo)) |
    ('checkBinOp ('andop, 'YesNo, 'YesNo) := 'someAType ('YesNo)) |
    ('checkBinOp ('orop, 'YesNo, 'YesNo) := 'someAType ('YesNo)) |
    ('checkBinOp ('eqop, 'Number, 'Number) := 'someAType ('YesNo)) |
    ('checkBinOp ('eqop, 'YesNo, 'YesNo) := 'someAType ('YesNo)) |
    ('checkBinOp ('eqop, 'Text, 'Text) := 'someAType ('YesNo)) |
    ('checkBinOp ('op, 't1, 't2) := 'noAType)

  val checkUnOp = function('checkUnOp.>>('UnOp, 'AType) -> 'OptAType) where
    ('checkUnOp ('notop, 'YesNo) := 'someAType ('YesNo)) |
    ('checkUnOp ('op, 't) := 'noAType)

  val echeck = function('echeck.>>('ATMap, 'Exp) -> 'OptAType) where
    ('echeck ('tm, 'constant ('B ('n))) := 'someAType ('YesNo)) |
    ('echeck ('tm, 'constant ('Num ('n))) := 'someAType ('Number)) |
    ('echeck ('tm, 'constant ('T ('n))) := 'someAType ('Text)) |
    ('echeck ('tm, 'qvar ('qid)) := 'lookupATMap ('qid, 'tm)) |
    ('echeck ('tm, 'binop ('e1, 'op, 'e2)) :=
      ((let('t1) := 'echeck ('tm, 'e1)) in
      ((let('t2) := 'echeck ('tm, 'e2)) in
        (iff (('isSomeAType ('t1) && 'isSomeAType ('t2)))
          th 'checkBinOp ('op, 'getAType ('t1), 'getAType ('t2))
          els 'noAType)))) |
    ('echeck ('tm, 'unop ('op, 'e)) :=
      ((let('t) := 'echeck ('tm, 'e)) in
        (iff ('isSomeAType ('t))
          th 'checkUnOp ('op, 'getAType ('t))
          els 'noAType)))

  val Tqempty = axiom(
    ===>("T-qempty")(
      ~'MC |- 'qempty :: 'MC ('atmempty, 'atmempty)))

  val Tquestion = axiom(
    ('lookupATMap (~'qid, ~'atm) === 'noAType
      ).===>("T-question")(
      'MC (~'atm, ~'qm) |- 'qsingle ('question (~'qid, ~'l, ~'at)) :: 'MC ('atmbind (~'qid, ~'at, 'atmempty), 'atmempty)))

  val Tvalue = axiom(
    (('lookupATMap (~'qid, ~'atm) === 'noAType) &
      ('echeck (~'atm, ~'exp) === 'someAType (~'at))
      ).===>("T-value")(
        'MC (~'atm, ~'qm) |- 'qsingle ('value (~'qid, ~'at, ~'exp)) :: 'MC ('atmbind (~'qid, ~'at, 'atmempty), 'atmempty)))

  val Tdefquestion = axiom(
    ('lookupATMap (~'qid, ~'qm) === 'noAType
      ).===>("T-defquestion")(
      'MC (~'atm, ~'qm) |- 'qsingle ('defquestion (~'qid, ~'l, ~'at)) :: 'MC ('atmempty, 'atmbind (~'qid, ~'at, 'atmempty))))

  // TODO is this correct?
  val Task = axiom(
    (('lookupATMap (~'qid, ~'qm) === 'someAType (~'at)) &
      ('lookupATMap (~'qid, ~'atm) === 'noAType)
      ).===>("T-ask")(
     'MC (~'atm, ~'qm) |- 'qsingle ('ask (~'qid)) :: 'MC ('atmbind (~'qid, ~'at, 'atmempty), 'atmempty)))

  val Tqseq = axiom(
    (('MC (~'atm, ~'qm) |- ~'q1 :: 'MC (~'atm1, ~'qm1)) &
      ('MC ('appendATMap (~'atm, ~'atm1), 'appendATMap (~'qm, ~'qm1)) |- ~'q2 :: 'MC (~'atm2, ~'qm2))
      ).===>("T-qseq")(
      'MC (~'atm, ~'qm) |- 'qseq (~'q1, ~'q2) :: 'MC ('appendATMap (~'atm1, ~'atm2), 'appendATMap (~'qm1, ~'qm2))))

  val Tqcond = axiom(
    (('echeck (~'atm, ~'exp) === 'someAType ('YesNo)) &
      ('MC (~'atm, ~'qm) |- ~'q1 :: 'MC (~'atm1, ~'qm1)) &
      ('MC (~'atm, ~'qm) |- ~'q2 :: 'MC (~'atm2, ~'qm2))
      ).===>("T-qcond")(
      'MC (~'atm, ~'qm) |- 'qcond (~'exp, ~'q1, ~'q2) :: 'MC ('intersectATM (~'atm1, ~'atm2), 'intersectATM (~'qm1, ~'qm2))))

  val Tqgroup = axiom(
    ('MC (~'atm, ~'qm) |- ~'q :: 'MC (~'atm1, ~'qm1)
      ).===>("T-qgroup")(
      'MC (~'atm, ~'qm) |- 'qgroup (~'gid, ~'q) :: 'MC (~'atm1, ~'qm1)))

  val qcCheck = function('qcCheck.>>('MapConf, 'QConf, 'ATMap) -> 'Bool)

  val TqcCheck = axiom(
    (('typeAM (~'am) === ~'atm1) &
      ('MC ('appendATMap (~'atm0, ~'atm1), 'appendATMap (~'qm0, 'typeQM (~'qm))) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-qcCheck")(
      'qcCheck ('MC (~'atm0, ~'qm0), 'QC (~'am, ~'qm, ~'q), 'appendATMap (~'atm1, ~'atm2))))


  val QLTypeSystem = Module("QLTypeSystem", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics)),
    Seq(ATMap, lookupATMap, appendATMap, MapConf, intersectATM, OptMapConf, isSomeMapConf, getMapConf, typeAM, typeQM, checkBinOp, checkUnOp, echeck,
      Tqempty, Tquestion, Tvalue, Tdefquestion, Task, Tqseq, Tqcond, Tqgroup, qcCheck, TqcCheck))

  val Tinvgeneral = axiom(
    (~'mc1 |- ~'q :: ~'mc2
      ).===>("T-inv-general")(
       exists(~'atm1, ~'qm1, ~'atm2, ~'qm2) |
         (~'mc1 === 'MC (~'atm1, ~'qm1)) &
           (~'mc2 === 'MC (~'atm2, ~'qm2))))

  val Tinv = axiom(
    ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2)
    ).===>("T-inv")(
     OR(
       =>>(~'q === 'qemtpy & ~'atm2 === 'atmempty & ~'qm2 === 'atmempty) |
       =>>(exists(~'qid, ~'l, ~'at) |
         (~'q === 'qsingle ('question (~'qid, ~'l, ~'at))) &
         (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
         (~'qm2 === 'atmempty) &
         ('lookupATMap (~'qid, ~'at) === 'noAType)) |
       =>>(exists(~'qid, ~'at, ~'exp) |
         (~'q === 'qsingle ('value (~'qid, ~'at, ~'exp))) &
         (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
         (~'qm2 === 'atmempty) &
         ('lookupATMap (~'qid, ~'at) === 'noAType) &
         ('echeck (~'atm1, ~'exp) === 'someAType (~'at))) |
       =>>(exists(~'qid, ~'l, ~'at) |
         (~'q === 'qsingle ('defquestion (~'qid, ~'l, ~'at))) &
         (~'atm2 === 'atmempty) &
         (~'qm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
         ('lookupATMap (~'qid, ~'qm1) === 'noAType)) |
       =>>(exists(~'qid, ~'at) |
         (~'q === 'qsingle ('ask (~'qid))) &
         (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
         (~'qm2 === 'atmempty) &
         ('lookupATMap (~'qid, ~'qm1) === 'someAType (~'at)) &
         ('lookupATMap (~'qid, ~'atm1) === 'noAType)) |
       =>>(exists(~'q1, ~'q2, ~'atmq1, ~'atmq2, ~'qmq1, ~'qmq2) |
         (~'q === 'qseq (~'q1, ~'q2)) &
         (~'atm2 === 'appendATMap (~'atmq1, ~'atmq2)) &
         (~'qm2 === 'appendATMap (~'qmq1, ~'qmq2)) &
         ('MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atmq1, ~'qmq1)) &
         ('MC ('appendATMap (~'atm1, ~'atmq1), 'appendATMap (~'qm1, ~'qmq1)) |- ~'q2 :: 'MC (~'atmq2, ~'qmq2))) |
       =>>(exists(~'exp, ~'q1, ~'q2, ~'atmq1, ~'atmq2, ~'qmq1, ~'qmq2) |
         (~'q === 'qcond (~'exp, ~'q1, ~'q2)) &
         (~'atm2 === 'intersectATM (~'atmq1, ~'atmq2)) &
         (~'qm2 === 'intersectATM (~'qmq1, ~'qmq2)) &
         ('echeck (~'atm1, ~'exp) === 'someAType ('YesNo)) &
         ('MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atmq1, ~'qmq1)) &
         ('MC (~'atm1, ~'qm1) |- ~'q2 :: 'MC (~'atmq2, ~'qmq2))) |
       =>>(exists(~'gid, ~'q1, ~'atmq1, ~'qmq1) |
         (~'q === 'qgroup (~'gid, ~'q1)) &
         (~'atm2 === ~'atmq1) &
         (~'qm2 === ~'qmq1) &
         ('MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atmq1, ~'qmq1))))))

  val Tinvqempty = lemma(
    ((~'q === 'qempty) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-q-empty")(
      (~'atm2 === 'atmempty) &
        (~'qm2 === 'atmempty)))

  val Tinvqsinglequestion = lemma(
    ((~'q === 'qsingle ('question (~'qid, ~'l, ~'at))) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qsingle-question")(
      (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
      (~'qm2 === 'atmempty) &
      ('lookupATMap (~'qid, ~'atm1) === 'noAType)))

  val Tinvqsinglevalue = lemma(
    ((~'q === 'qsingle ('value (~'qid, ~'at, ~'exp))) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qsingle-value")(
      (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
      (~'qm2 === 'atmempty) &
      ('lookupATMap (~'qid, ~'atm1) === 'noAType) &
      ('echeck (~'atm1, ~'exp) === 'someAType (~'at))))

  val Tinvqsingledefquestion = lemma(
    ((~'q === 'qsingle ('defquestion (~'qid, ~'l, ~'at))) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qsingle-defquestion")(
      (~'atm2 === 'atmempty) &
      (~'qm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
      ('lookupATMap (~'qid, ~'qm1) === 'noAType)))

  val Tinvqsingleask = lemma(
    ((~'q === 'qsingle ('ask (~'qid))) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qsingle-ask")(
      (exists(~'at) |
        (~'atm2 === 'atmbind (~'qid, ~'at, 'atmempty)) &
        (~'qm2 === 'atmempty) &
          ('lookupATMap (~'qid, ~'qm1) === 'someAType (~'at)) &
            ('lookupATMap (~'qid, ~'atm1) === 'noAType))))

  val Tinvqseq = lemma(
    ((~'q === 'qseq (~'q1, ~'q2)) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qseq")(
      (exists(~'atmq1, ~'atmq2, ~'qmq1, ~'qmq2) |
        (~'atm2 === 'appendATMap (~'atmq1, ~'atmq2)) &
        (~'qm2 === 'appendATMap (~'qmq1, ~'qmq2)) &
        ('MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atmq1, ~'qmq1)) &
        ('MC ('appendATMap (~'atm1, ~'atmq1), 'appendATMap (~'qm1, ~'qmq1)) |- ~'q2 :: 'MC (~'atmq2, ~'qmq2)))))

  val Tinvqcond = lemma(
    ((~'q === 'qcond (~'exp, ~'q1, ~'q2)) &
    ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
    ).===>("T-inv-qcond")(
      (exists(~'atmq1, ~'atmq2, ~'qmq1, ~'qmq2) |
        (~'atm2 === 'intersectATM (~'atmq1, ~'atmq2)) &
        (~'qm2 === 'intersectATM (~'qmq1, ~'qmq2)) &
        ('echeck (~'atm1, ~'exp) === 'someAType ('YesNo)) &
        ('MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atmq1, ~'qmq1)) &
        ('MC (~'atm1, ~'qm1) |- ~'q2 :: 'MC (~'atmq2, ~'qmq2)))))

  val Tinvqgroup = lemma(
    ((~'q === 'qgroup (~'gid, ~'q1)) &
      ('MC (~'atm1, ~'qm1) |- ~'q :: 'MC (~'atm2, ~'qm2))
      ).===>("T-inv-qgroup")(
      'MC (~'atm1, ~'qm1) |- ~'q1 :: 'MC (~'atm2, ~'qm2)))

  val TinvqcCheck = axiom(
    ('qcCheck ('MC (~'atm0, ~'qm0), 'QC (~'am, ~'qm, ~'q), ~'atm)
      ).===>("T-inv-qcCheck")(
      (exists(~'atm1, ~'atm2, ~'qm2) |
        ('typeAM (~'am) === ~'atm1) &
        ('MC ('appendATMap (~'atm0, ~'atm1), 'appendATMap (~'qm0, 'typeQM (~'qm))) |- ~'q :: 'MC (~'atm2, ~'qm2)) &
          (~'atm === 'appendATMap (~'atm1, ~'atm2)))))

  val QLTypeSystemInv = Module("QLTypeSystemInv", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem)),
    Seq(Tinvgeneral, Tinv, Tinvqempty, Tinvqsinglequestion, Tinvqsinglevalue, Tinvqsingledefquestion, Tinvqsingleask, Tinvqseq, Tinvqcond, Tinvqgroup, TinvqcCheck))

  val counterexample1: Goals = goal(
    (~'atm === 'atmbind (~'qid, ~'t, 'atmempty)
      ).===>("counterexample-1")(
      (exists(~'atm2) | ('intersectATM (~'atm, ~'atm2) ~= 'atmempty))))

  val counterexample2: Goals = goal(
    ((~'atm ~= 'atmempty)
      ).===>("counterexample-2")(
      (exists(~'qid) | ('lookupATMap (~'qid, ~'atm) ~= 'noAType))))

  val counterexample3: Goals = goal(
    ((~'q ~= 'qsingle ('value (~'qid1, 'Number, 'qvar (~'qid2)))) &
      (~'am1 === 'abind (~'qid2, 'Num ('zero), 'aempty))
      ).===>("counterexample-3")(
      (exists(~'am2) | ('reduce ('QC (~'am1, ~'qm, ~'q)) ~= 'reduce ('QC (~'am2, ~'qm, ~'q))))))

  val counterexample4: Goals = goal(
    ((~'exp ~= 'binop ('qvar (~'qid1), 'mulop, 'qvar (~'qid2))) &
      (~'am1 === 'abind (~'qid1, 'Num ('zero), 'abind (~'qid2, 'Num ('succ ('zero)), 'aempty)))
      ).===>("counterexample-4")(
      (exists(~'am2) | ('reduceExp (~'exp, ~'am1) ~= 'reduceExp (~'exp, ~'am2)))))

  val counterexample5: Goals = goal(
    ((~'q1 === 'qsingle ('question (~'qid1, ~'l1, ~'t1)))
      ).===>("counterexample-5")(
      (exists(~'am) |
        ('lookupAnsMap (~'qid1, ~'am) ~= 'noAval) &
          ('reduce ('QC (~'am, 'qmempty, ~'q1)) ~= 'noQConf))))

  val counterexample6: Goals = goal(
    ((~'q1 === 'qsingle ('ask (~'qid1)))
      ).===>("counterexample-6")(
      (exists(~'qm) |
        ('reduce ('QC ('aempty, 'qm, ~'q)) ~= 'noQConf))))

  val counterexample7: Goals = goal(
    ===>("counterexample-7")(
    (exists(~'atm) |
      ('echeck (~'atm, 'qvar (~'qid)) ~= 'noAType))))

  val counterexample8: Goals = goal(
    ===>("counterexample-8")(
    (exists(~'bo, ~'at1, ~'at2) |
      ('checkBinOp (~'bo, ~'at1, ~'at2) ~= 'noAType))))

  val counterexample9: Goals = goal(
    ((~'q === 'qsingle ('ask (~'qid)))
      ).===>("counterexample-9")(
      (exists(~'qm1, ~'qm2) |
        ('reduce ('QC ('aempty, ~'qm1, ~'q)) ~= 'reduce ('QC ('aempty, ~'qm2, ~'q))))))

  val counterexample10: Goals = goal(
    ===>("counterexample-10")(
    (exists(~'qm1, ~'qm2) |
      ('lookupQMap (~'qid, ~'qm1) ~= 'lookupQMap (~'qid, ~'qm2)))))

  val CounterExamples = Module("CounterExamples", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem)),
    Seq(counterexample1, counterexample2, counterexample3, counterexample4, counterexample5, counterexample6, counterexample7, counterexample8, counterexample9, counterexample10))


  val execution1: Local = local(
    differentconsts(
      'qid ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'singleq === 'qsingle ('question ('qid, 'l, 'Number))) &
        (~'am === 'aempty) &
        (~'qm === 'qmempty)
      ).===>("execution-1")(
      (exists(~'result) |
        'reduce ('QC (~'am, ~'qm, ~'singleq)) === ~'result))))

  val execution2: Goals = goal(
     ===>("execution-2")(
       (exists(~'result) | 'reduceExp ('unop ('notop, 'constant ('B ('yes))), 'aempty) === ~'result)))

  val execution3: Goals = goal(
    ===>("execution-3")(
      (exists(~'result) | 'evalBinOp ('divop ('Num ('succ ('zero)), 'Num ('succ ('zero)))) === ~'result)))

  val execution4: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'qid3 ::> 'QID,
      'av1 ::> 'Aval,
      'av2 ::> 'Aval,
      'av3 ::> 'Aval),
    goal(
      ((~'am1 === 'abind ('qid1, 'av1, 'abind ('qid2, 'av2, 'aempty))) &
      (~'am2 === 'abind ('qid3, 'av3, 'aempty))
      ).===>("execution-4")(
        (exists(~'result) |
          'appendAnsMap (~'am1, ~'am2) === ~'result))))

  val execution5: Goals = goal(
    (~'qcconf === 'someQConf (~'qc)
    ).===>("execution-5")(
      (exists(~'result) | 'getQC (~'qcconf) === ~'result)))

  val execution6: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'l1 ::> 'Label,
      'l2 ::> 'Label),
    goal(
      ((~'qm === 'qmbind ('qid1, 'l1, 'YesNo, 'qmbind ('qid2, 'l2, 'Number, 'qmempty))) &
        (~'q === 'qsingle ('ask ('qid2)))
        ).===>("execution-6")(
        (exists(~'result) |
          'reduce ('QC (~'am, ~'qm, ~'q)) === ~'result))))

  val execution7: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'l1 ::> 'Label,
      'l2 ::> 'Label),
    goal(
      ((~'q1 === 'qsingle ('question ('qid1, 'l1, 'Number))) &
        (~'q2 === 'qsingle ('question ('qid2, 'l2, 'Number))) &
        (~'q2 === 'qcond ('constant ('B ('yes), ~'q1, ~'q2)))
        ).===>("execution-7")(
        (exists(~'result) |
          'reduce ('QC ('aempty, 'qmempty, ~'q)) === ~'result))))

  val execution8: Goals = goal(
    ((~'e1 === 'constant ('B ('no))) &
      (~'e2 === 'constant ('B ('no))) &
      (~'am === 'aempty)
      ).===>("execution-8")(
      (exists(~'result) | 'reduceExp ('binop (~'e1, 'andop, ~'e2), ~'am) === ~'result)))

  val execution9: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'qid3 ::> 'QID,
      'l1 ::> 'Label,
      'l2 ::> 'Label,
      'l3 ::> 'Label),
    goal(
      ((~'qm === 'qmbind ('qid1, 'l1, 'Number, 'qmbind ('qid2, 'l2, 'Test, 'qmbind ('qid3, 'l3, 'YesNo))))
        ).===>("execution-9")(
        (exists(~'result) |
          'lookupQMap ('qid3, ~'qm) === ~'result))))

  val execution10: Local = local(
    differentconsts(
      't1 ::> 'string,
      't2 ::> 'string),
    goal(
      ===>("execution-10")(
        (exists(~'result) |
          'evalBinOp ('eqop, 'T ('t1), 'T ('t2)) === ~'result))))

  val Executions = Module("Execution", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem)),
    Seq(execution1, execution2, execution3, execution4, execution5, execution6, execution7, execution8, execution9, execution10))

  val proof1: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID),
    goal(
      ((~'am1 === 'abind ('qid1, ~'av1, 'abind ('qid2, ~'av2, 'aempty))) &
        (~'am2 === 'abind ('qid2, ~'av2, 'abind ('qid1, ~'av1, 'aempty)))
        ).===>("proof-1")(
          'lookupAnsMap ('qid1, ~'am1)  === 'lookupAnsMap ('qid2, ~'am2))))

  val proof2: Goals = goal(
    ((~'q === 'qsingle ('question (~'qid, ~'l, ~'t))) &
      ('reduce ('QC (~'am1, ~'qm1, ~'q)) === 'someQConf ('QC (~'am2, ~'qm2, 'qempty)))
      ).===>("proof-2")(
      ('isSomeAval ('lookupAnsMap (~'qid, ~'am2))) &
        ('typeOf ('getAval ('lookupAnsMap (~'qid, ~'am2))) === ~'t)))

  val proof3: Goals = goal(
    ((~'dq === 'qsingle ('defquestion (~'qid, ~'l, ~'t))) &
      ('reduce ('QC (~'am, ~'qm, ~'dq)) === 'someQConf ('QC (~'am1, ~'qm1, ~'eq))) &
      (~'aq == 'qsingle ('ask (~'qid)))
      ).===>("proof-3")(
      'isSomeQC ('reduce ('QC (~'am1, ~'qm1, ~'aq)))))

  val proof4: Goals = goal(
    ((~'q === 'qsingle ('question (~'qid, ~'l, 'Number))) &
      ('reduce ('QC (~'am1, ~'qm1, ~'q)) === 'someQConf ('QC (~'am2, ~'qm2, 'qempty))) &
      (~'exp === 'binop ('qvar (~'qid1), 'addop, 'constant ('Num ('succ ('zero))))) &
      (~'qexp == 'qsingle ('value (~'qid2, 'Number, ~'exp)))
      ).===>("proof-4")(
      'isSomeQC ('reduce ('QC (~'am2, ~'qm2, ~'qexp)))))

  val proof5: Goals = goal(
    ((~'q1 === 'qsingle ('fquestion (~'qid1, ~'l1, ~'t1))) &
      (~'q2 === 'qsingle ('fquestion (~'qid2, ~'l2, ~'t2))) &
      (~'qs === 'qseq (~'q1, ~'q2)) &
      ('MC ('atmempty, 'atmempty) |- ~'qs :: 'MC (~'atm, ~'qm))
      ).===>("proof-5")(
      ~'qid1 ~= ~'qid2))

  val proof6: Goals = goal(
    ((~'dq1 === 'qsingle ('defquestion (~'qid1, ~'l1, ~'t1))) &
      (~'dq2 === 'qsingle ('defquestion (~'qid2, ~'l2, ~'t2))) &
      (~'qs === 'qseq (~'dq1, ~'dq2)) &
      ('MC ('atmempty, 'atmempty) |- ~'qs :: 'MC (~'atm, ~'qm))
      ).===>("proof-6")(
      ~'qid1 ~= ~'qid2))

  val proof7: Goals = goal(
    ((~'q1 === 'qsingle ('question (~'qid1, ~'l1, ~'t1))) &
      (~'menv === 'MC ('atmempty, 'atmempty)) &
      (~'menv |- ~'q1 :: 'MC (~'atm, ~'qm)) &
      ('reduce ('QC ('aempty, 'qmempty, ~'q1)) === 'someQConf (~'qc))
      ).===>("proof-7")(
      'qcCheck (~'menv, ~'qc, ~'atm)))

  val proof8: Goals = goal(
    ((~'amt1 === 'atmbind (~'qid1, ~'t1, 'atmempty)) &
      (~'amt2 === 'atmbind (~'qid2, ~'t2, 'atmempty))
      ).===>("proof-8")(
      'intersectATM (~'atm1, ~'atm2)  === 'intersectATM (~'atm2, ~'atm1)))

  val proof9: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID),
    goal(
      ((~'q1 === 'qsingle ('question ('qid1, ~'l1, ~'t1))) &
        (~'q2 === 'qsingle ('question ('qid2, ~'l2, ~'t2))) &
        (~'qcond === 'qcond (~'exp, ~'q1, ~'q2)) &
        ('echeck ('atmempty, ~'exp) === 'someAType ('YesNo))
        ).===>("proof-9")(
        ('MC ('atmempty, 'atmempty) |- ~'qcond :: 'MC ('atmempty, 'atmempty)))))


  val proof10: Goals = goal(
    ('evalBinOp (~'op, ~'av1, ~'av2) === 'someExp ('constant (~'avr))
      ).===>("proof-10")(
      ('typeOf (~'av1) === 'typeOf (~'av2)) &
      ('typeOf (~'avr) === 'typeOf (~'var))))

  val Proofs = Module("Proofs", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem), Resolved(QLTypeSystemInv)),
    Seq(proof1, proof2, proof3, proof4, proof5, proof6, proof7, proof8, proof9, proof10))


  val synthesis1: Goals = goal(
    ===>("synthesis-1")(
      (exists(~'qc) |
        'reduce (~'qc) === 'noQConf)))

  val synthesis2: Goals = goal(
    ===>("synthesis-2")(
      (exists(~'exp) |
        (!'expIsValue (~'exp)) &
          ('reduceExp (~'exp, 'aempty) === 'noExp))))

  val synthesis3: Goals = goal(
    ===>("synthesis-3")(
      (exists(~'av1, ~'av2) |
        'evalBinOp ('addop, ~'av1, ~'av2) === 'noExp)))

  val synthesis4: Local = local(
    consts(
      'am ::> 'AnsMap,
      'qm ::> 'QMap,
      'sq ::> 'Questionnaire),
    goal(
      ===>("synthesis-4")(
        (exists(~'q) |
          ('reduce ('QC ('am, 'qm, ~'q)) === 'someQConf ('QC ('am, 'qm, 'sq)))))))

  val synthesis5: Goals = goal(
    ===>("synthesis-5")(
      (exists(~'atm1, ~'atm1) |
        (~'atm1 ~= 'atmempty) &
          (~'atm2 ~= 'atmempty) &
          ('intersectATM (~'atm1, ~'atm2) === 'atmempty))))

  val synthesis6: Local = local(
    differentconsts(
      'qid ::> 'QID,
      'av ::> 'Aval),
    goal(
      ===>("synthesis-6")(
        (exists(~'am) |
          ('lookupAnsMap ('qid, ~'am) === 'someAval ('av))))))

  val synthesis7: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'qid3 ::> 'QID),
    goal(
      (~'atm === 'atmbind ('qid1, 'Number, 'atmbind ('qid2, 'Text, 'atmbind ('qid3, 'YesNo, 'atmempty)))
      ).===>("synthesis-7")(
        (exists(~'qm) |
          ('typeQM ('qm) === ~'atm)))))

  val synthesis8: Local = local(
    consts(
      'am ::> 'AnsMap,
      'qm ::> 'QMap,
      'qid ::> 'QID),
    goal(
      ((~'e === 'binop ('constant ('Num ('zero)), 'addop, 'constant ('Num ('succ ('zero))))) &
        (~'qcres === 'someQConf ('QC ('am, 'qm, 'qsingle ('value ('qid, 'Number (~'e))))))
        ).===>("synthesis-8")(
        (exists(~'qc) |
          ('reduce ('qc) === ~'qcres)))))

  val synthesis9: Goals = goal(
    ===>("synthesis-9")(
      (exists(~'n1, ~'n1) |
          'minus (~'n1, ~'n2) === 'zero)))

  val synthesis10: Goals = goal(
    ===>("synthesis-10")(
      (exists(~'e) |
        'echeck ('atmempty, ~'e) === 'noAType)))


  val Syntheses = Module("Syntheses", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem)),
    Seq(synthesis1, synthesis2, synthesis3, synthesis4, synthesis5, synthesis6, synthesis7, synthesis8, synthesis9, synthesis10))

  val test1: Goals = goal(
    ((~'two === 'succ ('succ ('zero))) &
      (~'four === 'succ ('succ (~'two)))
      ).===>("test-1")(
      'multiply (~'two, ~'two) === ~'four))

  val test2: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID),
    goal(
      (~'am === 'abind ('qid1, 'Num ('zero), 'abind ('qid2, 'Num ('succ ('zero)), 'aempty))
        ).===>("test-2")(
        ('lookupAnsMap ('qid, ~'am) === 'someAval ('Num ('succ ('zero)))))))

  val test3: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'am === 'aempty) &
        (~'qm === 'qmbind ('qid1, 'l, 'YesNo, 'qmempty)) &
        (~'q === 'qsingle ('question ('qid1, 'l, 'YesNO)))
        ).===>("test-3")(
        ('reduce ('QC (~'am, ~'qm, ,'qsingle ('ask ('qid1)))) === 'someQConf ('QC (~'am, ~'qm, ~'q))))))

  val test4: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'am === 'aempty) &
        (~'qm === 'qmbind ('qid1, 'l, 'YesNo, 'qmempty)) &
        (~'q === 'qsingle ('question ('qid1, 'l, 'YesNO)))
        ).===>("test-4")(
        ('reduce ('QC (~'am, ~'qm, ,'qsingle ('ask ('qid2)))) === 'noQConf))))

  val test5: Goals = goal(
    (~'am === 'abind (~'qid, 'B ('yes), 'aempty)
      ).===>("test-5")(
      'reduceExp ('qvar (~'qid), ~'am) === 'someExp ('constant ('B ('yes)))))

  val test6: Goals = goal(
    ===>("test-6")(
      'evalBinOp ('andop,  'B ('yes), 'B ('no)) === 'someExp ('constant ('B ('no)))))

  val test7: Local = local(
    differentconsts(
      'qid1 ::> 'QID,
      'qid2 ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'e1 === 'qvar ('qid1)) &
        (~'e2 === 'qvar ('qid2)) &
        (~'am === 'abind ('qid1 , 'B ('yes), 'abind ('qid2, 'B ('yes), 'aempty)))
        ).===>("test-7")(
        ('reduceExp ('binop (~'e1, 'orop, ~'e2), ~'am) === 'someExp ('binop ('constant ('B ('yes)), 'orop, ~'e2))))))

  val test8: Local = local(
    differentconsts(
      'qid ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'q === 'qsingle ('defquestion ('qid, 'l, 'Text))) &
        (~'nqm === 'qmbind ('qid, 'l, 'Text, ~'qm))
        ).===>("test-8")(
        ('reduce ('QC (~'am, ~'qm, ~'q)) === 'someQConf ('QC (~'am, ~'nqm, 'qempty))))))

  val test9: Local = local(
    differentconsts(
      'qid ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'q === 'qsingle ('question ('qid, 'l, 'Number))) &
        (~'atmr === 'atmbind ('qid, 'Number, 'atmempty))
        ).===>("test-9")(
        ('MC ('atmempty, 'atmempty) |- ~'q :: 'MC (~'atmr, 'atmempty)))))

  val test10: Local = local(
    differentconsts(
      'qid ::> 'QID,
      'l ::> 'Label),
    goal(
      ((~'q1 === 'qsingle ('defquestion ('qid, 'l, 'YesNo))) &
        (~'q2 === 'qsingle ('ask ('qid))) &
        (~'qs === 'qseq (~'q1, ~'q2)) &
        (~'atm1 === 'atmempty) &
        (~'atm2 === 'atmbind ('qid, 'YesNo, 'atmempty)) &
        (~'qm1 === 'atmbind ('qid, 'YesNo, 'atmempty)) &
        (~'qm2 === 'atmempty)
        ).===>("test-10")(
        ('MC ('atmempty, 'atmempty) |- 'qseq (~'q1, ~'q2) :: 'MC ('appendATMap (~'atm1, ~'atm2), 'appendATMap (~'qm1, ~'qm2))))))

  val Tests = Module("Tests", Seq(Resolved(BasicTypes), Resolved(QLSyntax), Resolved(QLSemanticsData), Resolved(QLSemantics), Resolved(QLTypeSystem)),
    Seq(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10))

}
