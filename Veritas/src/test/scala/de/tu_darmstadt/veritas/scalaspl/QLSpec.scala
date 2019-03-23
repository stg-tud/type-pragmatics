package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object QLSpec extends ScalaSPLSpecification {

  // BasicTypes
  sealed trait YN extends Expression
  case class yes() extends YN
  case class no() extends YN

  def and(a: YN, b: YN): YN = (a, b) match {
    case (yes(), yes()) => yes()
    case (_, _) => no()
  }

  def or(a: YN, b: YN): YN = (a, b) match {
    case (no(), no()) => no()
    case (_, _) => yes()
  }

  def not(a: YN): YN = a match {
    case yes() => no()
    case no() => yes()
  }

  sealed trait nat extends Expression
  case class zero() extends nat
  case class succ(n: nat) extends nat

  def pred(n: nat): nat = n match {
    case zero() => zero()
    case succ(n) => n
  }

  def gt(a: nat, b: nat): YN = (a, b) match {
    case (zero(), _) => no()
    case (succ(_), zero()) => yes()
    case (succ(n1), succ(n2)) => gt(n1, n1)
  }

  def lt(a: nat, b: nat): YN = (a, b) match {
    case (_, zero()) => no()
    case (zero(), succ(_)) => yes()
    case (succ(n1), succ(n2)) => lt(n1, n1)
  }

  def plus(a: nat, b: nat): nat = (a, b) match {
    case (n, zero()) => n
    case (n1, succ(n2)) => succ(plus(n1, n2))
  }

  def minus(a: nat, b: nat): nat = (a, b) match {
    case (n, zero()) => n
    case (n1, succ(n2)) => pred(minus(n1, n2))
  }

  def multiply(a: nat, b: nat): nat = (a, b) match {
    case (_, zero()) => zero()
    case (n1, succ(n2)) => plus(n1, multiply(n1, n2))
  }

  def divide(a: nat, b: nat): nat = (a, b) match {
    case (n1, n2) =>
      if(gt(n1, n2) == yes())
        succ(divide(minus(n1, n2), n2))
      else
        zero()
  }

  trait char extends Expression

  sealed trait string extends Expression
  case class sempty() extends string
  case class scons(c: char, tail: string) extends string

  // QLSyntax
  trait QID extends Expression

  trait GID extends Expression

  trait Label extends Expression

  sealed trait Aval extends Expression
  case class B(value: YN) extends Aval
  case class Num(value: nat) extends Aval
  case class T(value: string) extends Aval

  @FailableType
  sealed trait OptAval
  case class noAval() extends OptAval
  case class someAval(value: Aval) extends OptAval

  def isSomeAval(opt: OptAval): Boolean = opt match {
    case noAval() => false
    case someAval(_) => true
  }

  @Partial
  def getAval(opt: OptAval): Aval = opt match {
    case someAval(aval) => aval
  }

  sealed trait AType extends Expression with Type
  case class YesNo() extends AType
  case class Number() extends AType
  case class Text() extends AType

  @FailableType
  sealed trait OptAType
  case class noAType() extends OptAType
  case class someAType(typ: AType) extends OptAType

  def isSomeAType(opt: OptAType): Boolean = opt match {
    case noAType() => false
    case someAType(_) => true
  }

  @Partial
  def getAType(opt: OptAType): AType = opt match {
    case someAType(atype) => atype
  }

  @Static
  def typeOf(aval: Aval): AType = aval match {
    case B(_) => YesNo()
    case Num(_) => Number()
    case T(_) => Text()
  }

  sealed trait ATList extends Expression
  case class atempty() extends ATList
  case class atcons(atype: AType, rem: ATList) extends ATList

  def append(atl1: ATList, atl2: ATList): ATList = (atl1, atl2) match {
    case (atempty(), atl) => atl
    case (atcons(atype, atlr), atl) => atcons(atype, append(atlr, atl))
  }

  sealed trait BinOpT extends Expression
  case class addop() extends BinOpT
  case class subop() extends BinOpT
  case class mulop() extends BinOpT
  case class divop() extends BinOpT
  case class eqop() extends BinOpT
  case class gtop() extends BinOpT
  case class ltop() extends BinOpT
  case class andop() extends BinOpT
  case class orop() extends BinOpT

  sealed trait UnOpT extends Expression
  case class notop() extends UnOpT

  sealed trait Exp extends Expression
  case class constant(aval: Aval) extends Exp
  case class qvar(qid: QID) extends Exp
  case class binop(e1: Exp, op: BinOpT, e2: Exp) extends Exp
  case class unop(op: UnOpT, e: Exp) extends Exp

  sealed trait Entry extends Expression
  case class question(qid: QID, l: Label, at: AType) extends Entry
  case class value(qid: QID, at: AType, exp: Exp) extends Entry
  case class defquestion(qid: QID, l: Label, at: AType) extends Entry
  case class ask(qid: QID) extends Entry

  sealed trait Questionnaire extends Expression
  case class qempty() extends Questionnaire
  case class qsingle(entry: Entry) extends Questionnaire
  case class qseq(qs1: Questionnaire, qs2: Questionnaire) extends Questionnaire
  case class qcond(e: Exp, els: Questionnaire, thn: Questionnaire) extends Questionnaire
  case class qgroup(gid: GID, qs: Questionnaire) extends Questionnaire

  // QLSemanticsData

  sealed trait AnsMap extends Expression
  case class aempty() extends AnsMap
  case class abind(qid: QID, aval: Aval, al: AnsMap) extends AnsMap

  @Dynamic
  @ProgressProperty("lookupAnsMapProgress")
  @PreservationProperty("lookupAnsMapPreservation")
  @Recursive(1)
  def lookupAnsMap(id: QID, am: AnsMap): OptAval = (id, am) match {
    case (_, aempty()) => noAval()
    case (qid, abind(qid1, aval, aml)) =>
      if (qid == qid1)
        someAval(aval)
      else
        lookupAnsMap(qid, aml)
  }

  def appendAnsMap(am1: AnsMap, am2: AnsMap): AnsMap = (am1, am2) match {
    case (aempty(), am) => am
    case (abind(qid, av, am), aml) => abind(qid, av, appendAnsMap(am, aml))
  }

  sealed trait QMap extends Expression
  case class qmempty() extends QMap
  case class qmbind(qid: QID, l: Label, atype: AType, qml: QMap) extends QMap

  @FailableType
  sealed trait OptQuestion
  case class noQuestion() extends OptQuestion
  case class someQuestion(qid: QID, l: Label, atype: AType) extends OptQuestion

  def isSomeQuestion(opt: OptQuestion): Boolean = opt match {
    case noQuestion() => false
    case someQuestion(_, _, _) => true
  }

  @Partial
  def getQuestionQID(opt: OptQuestion): QID = opt match {
    case someQuestion(qid, l, at) => qid
  }

  @Partial
  def getQuestionLabel(opt: OptQuestion): Label = opt match {
    case someQuestion(qid, l, at) => l
  }

  @Partial
  def getQuestionAType(opt: OptQuestion): AType = opt match {
    case someQuestion(qid, l, at) => at
  }

  @Dynamic
  @ProgressProperty("lookupQMapProgress")
  @PreservationProperty("lookupQMapPreservation")
  @Recursive(1)
  def lookupQMap(id: QID, qm: QMap): OptQuestion = (id, qm) match {
    case (_, qmempty()) => noQuestion()
    case (qid, qmbind(qid1, l, at, qml)) =>
      if (qid == qid1)
        someQuestion(qid, l, at)
      else
        lookupQMap(qid, qml)
  }

  sealed trait QConf extends Expression
  case class QC(am: AnsMap, qm: QMap, q: Questionnaire) extends QConf

  def getAM(qc: QConf): AnsMap = qc match {
    case QC(am, _, _) => am
  }

  def getQM(qc: QConf): QMap = qc match {
    case QC(_, qm, _) => qm
  }

  def getQuest(qc: QConf): Questionnaire = qc match {
    case QC(_, _, q) => q
  }

  def isValue(q: Questionnaire): Boolean = q match {
    case qempty() => true
    case _ => false
  }

  @FailableType
  sealed trait OptQConf
  case class noQConf() extends OptQConf
  case class someQConf(qc: QConf) extends OptQConf

  def isSomeQC(opt: OptQConf): Boolean = opt match {
    case noQConf() => false
    case someQConf(_) => true
  }

  @Partial
  def getQC(opt: OptQConf): QConf = opt match {
    case someQConf(qc) => qc
  }

  @FailableType
  sealed trait OptExp
  case class noExp() extends OptExp
  case class someExp(exp: Exp) extends OptExp

  def isSomeExp(opt: OptExp): Boolean = opt match {
    case noExp() => false
    case someExp(_) => true
  }

  @Partial
  def getExp(opt: OptExp): Exp = opt match {
    case someExp(e) => e
  }

  def expIsValue(exp: Exp): Boolean = exp match {
    case constant(_) => true
    case e => false
  }

  @Partial
  def getExpValue(exp: Exp): Aval = exp match {
    case constant(av) => av
  }

  // QLSemantics

  def askYesNo(l: Label): YN = ???
  def askNumber(l: Label): nat = ???
  def askText(l: Label): string = ???

  def getAnswer(l: Label, at: AType): Aval = (l, at) match {
    case (l, YesNo()) => B(askYesNo(l))
    case (l, Number()) => Num(askNumber(l))
    case (l, Text()) => T(askText(l))
  }

  @Dynamic
  @ProgressProperty("evalBinOpProgress")
  @PreservationProperty("evalBinOpPreservation")
  @TopLevelDistinctionHint(0, 1, 2)
  def evalBinOp(op: BinOpT, av1: Aval, av2: Aval): OptExp = (op, av1, av2) match {
    case (addop(), Num(n1), Num(n2)) => someExp(constant(Num(plus(n1, n2))))
    case (subop(), Num(n1), Num(n2)) => someExp(constant(Num(minus(n1, n2))))
    case (mulop(), Num(n1), Num(n2)) => someExp(constant(Num(multiply(n1, n2))))
    case (divop(), Num(n1), Num(n2)) => someExp(constant(Num(divide(n1, n2))))
    case (gtop(), Num(n1), Num(n2)) => someExp(constant(B(gt(n1, n2))))
    case (ltop(), Num(n1), Num(n2)) => someExp(constant(B(lt(n1, n2))))
    case (andop(), B(b1), B(b2)) => someExp(constant(B(and(b1, b2))))
    case (orop(), B(b1), B(b2)) => someExp(constant(B(or(b1, b2))))
    case (eqop(), a, a1) =>
      if(a == a1)
        someExp(constant(B(yes())))
      else
        someExp(constant(B(no())))
    case (_, _, _) => noExp()
  }

  @Dynamic
  @ProgressProperty("evalUnOpProgress")
  @PreservationProperty("evalUnOpPreservation")
  def evalUnOp(op: UnOpT, av: Aval): OptExp = (op, av) match {
    case (notop(), B(b)) => someExp(constant(B(not(b))))
    case (_, _) => noExp()
  }

  @Dynamic
  @ProgressProperty("reduceExpProgress")
  @PreservationProperty("reduceExpPreservation")
  @Recursive(0)
  def reduceExp(exp: Exp, amap: AnsMap): OptExp = (exp, amap) match {
    case (constant(av), _) => noExp()
    case (qvar(qid), am) =>
      val avOpt = lookupAnsMap(qid, am)
      if (isSomeAval(avOpt))
        someExp(constant(getAval(avOpt)))
      else
        noExp()
    case (binop(e1, op, e2), am) =>
      if (expIsValue(e1))
        if (expIsValue(e2))
          evalBinOp(op, getExpValue(e1), getExpValue(e2))
        else {
          val eOpt2 = reduceExp(e2, am)
          if (isSomeExp(eOpt2))
            someExp(binop(e1, op, getExp(eOpt2)))
          else noExp()
        }
      else {
        val eOpt1 = reduceExp(e1, am)
        if (isSomeExp(eOpt1))
          someExp(binop(getExp(eOpt1), op, e2))
        else noExp()
      }
    case (unop(op, e1), am) =>
      if (expIsValue(e1))
        evalUnOp(op, getExpValue(e1))
      else {
        val eOpt = reduceExp(e1, am)
        if (isSomeExp(eOpt))
          someExp(unop(op, getExp(eOpt)))
        else noExp()
      }
  }

  @Recursive(0)
  @ProgressProperty("Progress")
  @PreservationProperty("Preservation")
  @Dynamic
  def reduce(q: Questionnaire, ama: AnsMap, qma: QMap): OptQConf = (q, ama, qma) match {
    case (qempty(), _, _) => noQConf()
    case (qsingle(question(qid, l, t)), am, qm) =>
      val av = getAnswer(l, t)
      someQConf(QC(abind(qid, av, am), qm, qempty()))
    case (qsingle(value(qid, t, exp)), am, qm) =>
      if (expIsValue(exp))
        someQConf(QC(abind(qid, getExpValue(exp), am), qm, qempty()))
      else {
        val eOpt = reduceExp(exp, am)
        if (isSomeExp(eOpt))
          someQConf(QC(am, qm, qsingle(value(qid, t, getExp(eOpt)))))
        else noQConf()
      }
    case (qsingle(defquestion(qid, l, t)), am, qm) =>
      someQConf(QC(am, qmbind(qid, l, t, qm), qempty()))
    case (qsingle(ask(qid)), am, qm) =>
      val qOpt = lookupQMap(qid, qm)
      if (isSomeQuestion(qOpt))
        someQConf(QC(am, qm, qsingle(question(qid,
          getQuestionLabel(qOpt),
          getQuestionAType(qOpt)))))
      else noQConf()
    case (qseq(qempty(), qs), am, qm) => someQConf(QC(am, qm, qs))
    case (qseq(qs1, qs2), am, qm) =>
      val qcOpt = reduce(qs1, am, qm)
      if (isSomeQC(qcOpt)) {
        val qc = getQC(qcOpt)
        someQConf(QC(getAM(qc), getQM(qc), qseq(getQuest(qc), qs2)))
      }
      else noQConf()
    case (qcond(constant(B(yes())), qs1, qs2), am, qm) => someQConf(QC(am, qm, qs1))
    case (qcond(constant(B(no())), qs1, qs2), am, qm) => someQConf(QC(am, qm, qs2))
    case (qcond(e, qs1, qs2), am, qm) =>
      val eOpt = reduceExp(e, am)
      if (isSomeExp(eOpt))
        someQConf(QC(am, qm, qcond(getExp(eOpt), qs1, qs2)))
      else noQConf()
    case (qgroup(_, qs), am, qm) => someQConf(QC(am, qm, qs))
  }

  // QLTypeSystem

  sealed trait ATMap extends Context with Type
  case class atmempty() extends ATMap
  case class atmbind(qid: QID, at: AType, atml: ATMap) extends ATMap

  @Static
  @Recursive(1)
  def lookupATMap(qid: QID, atm: ATMap): OptAType = (qid, atm) match {
    case (_, atmempty()) => noAType()
    case (qid1, atmbind(qid2, at, atml)) =>
      if (qid1 == qid2) someAType(at)
      else lookupATMap(qid1, atml)
  }

  @Static
  @Recursive(0)
  def appendATMap(atm1: ATMap, atm2: ATMap): ATMap = (atm1, atm2) match {
    case (atmempty(), atm) => atm
    case (atmbind(qid, at, atm), atml) => atmbind(qid, at, appendATMap(atm, atml))
  }

  @Static
  @Recursive(0)
  def intersectATM(atm1: ATMap, atm2: ATMap): ATMap = (atm1, atm2) match {
    case (atmempty(), _) => atmempty()
    //case (_, atmempty()) => atmempty() //shortcut case is a bit problematic with current ACG construction, maybe fix later
    case (atmbind(qid, at, atm1), atm2) =>
      val atm1atm2 = intersectATM(atm1, atm2)
      val lAT = lookupATMap(qid, atm2)
      if (isSomeAType(lAT) && getAType(lAT) == at)
        atmbind(qid, at, atm1atm2)
      else
        atm1atm2
  }

  sealed trait MapConf extends Context with Type
  case class MC(atm: ATMap, qtm: ATMap) extends MapConf

  @FailableType
  sealed trait OptMapConf
  case class noMapConf() extends OptMapConf
  case class someMapConf(mc: MapConf) extends OptMapConf

  def isSomeMapConf(opt: OptMapConf): Boolean = opt match {
    case noMapConf() => false
    case someMapConf(_) => true
  }

  @Partial
  def getMapConf(opt: OptMapConf): MapConf = opt match {
    case someMapConf(mc) => mc
  }

  @Static
  @Recursive(0)
  def typeAM(am: AnsMap): ATMap = am match {
    case aempty() => atmempty()
    case abind(qid, av, amr) => atmbind(qid, typeOf(av), typeAM(amr))
  }

  @Static
  @Recursive(0)
  def typeQM(qm: QMap): ATMap = qm match {
    case qmempty() => atmempty()
    case qmbind(qid, _, at, qmr) => atmbind(qid, at, typeQM(qmr))
  }

  def checkBinOp(op: BinOpT, at1: AType, at2: AType): OptAType = (op, at1, at2) match {
    case (addop(), Number(),  Number()) => someAType(Number())
    case (subop(), Number(),  Number()) => someAType(Number())
    case (mulop(), Number(),  Number()) => someAType(Number())
    case (divop(), Number(),  Number()) => someAType(Number())
    case (gtop(), Number(),  Number()) => someAType(YesNo())
    case (ltop(), Number(),  Number()) => someAType(YesNo())
    case (andop(), YesNo(),  YesNo()) => someAType(YesNo())
    case (orop(), YesNo(),  YesNo()) => someAType(YesNo())
    case (eqop(), _, _) => someAType(YesNo())
    case (_, _, _) => noAType()
  }

  @Static
  def checkUnOp(op: UnOpT, at: AType): OptAType = (op, at) match {
    case (notop(), YesNo()) => someAType(YesNo())
    case (_, _) => noAType()
  }

  @Static
  @Recursive(1)
  def echeck(atm: ATMap, exp: Exp): OptAType = (atm, exp) match {
    case (_, constant(B(n))) => someAType(YesNo())
    case (_, constant(Num(n))) => someAType(Number())
    case (_, constant(T(n))) => someAType(Text())
    case (atm, qvar(qid)) => lookupATMap(qid, atm)
    case (atm, binop(e1, op, e2)) =>
      val t1 = echeck(atm, e1)
      val t2 = echeck(atm, e2)
      if(isSomeAType(t1) && isSomeAType(t2))
        checkBinOp(op, getAType(t1), getAType(t2))
      else noAType()
    case (atm, unop(op, e)) =>
      val t = echeck(atm, e)
      if (isSomeAType(t))
        checkUnOp(op, getAType(t))
      else noAType()
  }

  @Axiom
  def Tqempty(atm: ATMap, qm: ATMap): Unit = {
  } ensuring MC(atm, qm) |- qempty() :: MC(atm, qm)

  @Axiom
  def Tquestion(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType): Unit = {
    require(lookupATMap(qid, atm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(question(qid, l, at)) :: MC(atmbind(qid, at, atm), qm)

  @Axiom
  def Tvalue(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType): Unit = {
    require(lookupATMap(qid, atm) == noAType())
    require(echeck(atm, exp) == someAType(at))
  } ensuring MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atmbind(qid, at, atm), qm)

  @Axiom
  def Tdefquestion(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType): Unit = {
    require(lookupATMap(qid, qm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(defquestion(qid, l, at)) :: MC(atm, atmbind(qid, at, qm))

  @Axiom
  def Task(qid: QID, qm: ATMap, at: AType, atm: ATMap): Unit = {
    require(lookupATMap(qid, qm) == someAType(at))
    require(lookupATMap(qid, atm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(ask(qid)) :: MC(atmbind(qid, at, atm), qm)

  @Axiom
  def Tqseq(atm: ATMap, qm: ATMap, q1: Questionnaire, atm1: ATMap,
      atm2: ATMap, qm1: ATMap, q2: Questionnaire, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm1, qm1) |- q2 :: MC(atm2, qm2))
  } ensuring MC(atm, qm) |- qseq(q1, q2) :: MC(atm2, qm2)

  @Axiom
  def Tqcond(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
      atm1: ATMap, qm1: ATMap, q2: Questionnaire): Unit = {
    require(echeck(atm, exp) == someAType(YesNo()))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm1, qm1))
  } ensuring MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atm1, qm1)

  @Axiom
  def Tqgroup(atm: ATMap, qm: ATMap, q: Questionnaire, atm1: ATMap, qm1: ATMap, gid: GID): Unit = {
    require(MC(atm, qm) |- q :: MC(atm1, qm1))
  } ensuring (MC(atm, qm) |- qgroup(gid, q) :: MC(atm1, qm1))


  //type inversion axioms
  @Axiom
  def Tqempty_inv1(atm: ATMap, qm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require (MC(atm, qm) |- qempty() :: MC(atm1, qm1))
  } ensuring (atm1 == atm)

  @Axiom
  def Tqempty_inv2(atm: ATMap, qm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require (MC(atm, qm) |- qempty() :: MC(atm1, qm1))
  } ensuring (qm1 == qm)


  @Axiom
  def Tquestion_inv1(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(question(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (lookupATMap(qid, atm) == noAType())

  @Axiom
  def Tquestion_inv2(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(question(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (atm1 == atmbind(qid, at, atm))

  @Axiom
  def Tquestion_inv3(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(question(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (qm1 == qm)


  @Axiom
  def Tvalue_inv1(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atm1, qm1))
  } ensuring (lookupATMap(qid, atm) == noAType())

  @Axiom
  def Tvalue_inv2(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atm1, qm1))
  } ensuring (echeck(atm, exp) == someAType(at))

  @Axiom
  def Tvalue_inv3(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atm1, qm1))
  } ensuring (atm1 == atmbind(qid, at, atm))

  @Axiom
  def Tvalue_inv4(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atm1, qm1))
  } ensuring (qm1 == qm)


  @Axiom
  def Tdefquestion_inv1(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require (MC(atm, qm) |- qsingle(defquestion(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (lookupATMap(qid, qm) == noAType())

  @Axiom
  def Tdefquestion_inv2(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require (MC(atm, qm) |- qsingle(defquestion(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (atm1 == atm)

  @Axiom
  def Tdefquestion_inv3(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType, atm1: ATMap, qm1: ATMap): Unit = {
    require (MC(atm, qm) |- qsingle(defquestion(qid, l, at)) :: MC(atm1, qm1))
  } ensuring (qm1 == atmbind(qid, at, qm))


  @Axiom
  def Task_inv1(qid: QID, qm: ATMap, at: AType, atm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(ask(qid)) :: MC(atm1, qm1))
  } ensuring (lookupATMap(qid, atm) == noAType())

  @Axiom
  def Task_inv2(qid: QID, qm: ATMap, atm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(ask(qid)) :: MC(atm1, qm1))
  } ensuring (exists ((at: AType) => lookupATMap(qid, qm) == someAType(at)))

  @Axiom
  def Task_inv3(qid: QID, qm: ATMap, at: AType, atm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(ask(qid)) :: MC(atm1, qm1))
    require(lookupATMap(qid, qm) == someAType(at))
  } ensuring (atm1 == atmbind(qid, at, atm))

  @Axiom
  def Task_inv4(qid: QID, qm: ATMap, atm: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qsingle(ask(qid)) :: MC(atm1, qm1))
  } ensuring (qm1 == qm)


  @Axiom
  def Tqseq_inv1(atm: ATMap, qm: ATMap, q1: Questionnaire, q2: Questionnaire, atmr: ATMap, qmr: ATMap): Unit = {
    require(MC(atm, qm) |- qseq(q1, q2) :: MC(atmr, qmr))
  } ensuring (exists ((atm1: ATMap, qm1: ATMap) => MC(atm, qm) |- q1 :: MC(atm1, qm1)))

  @Axiom
  def Tqseq_inv2(atm: ATMap, qm: ATMap, q1: Questionnaire, q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qseq(q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
  } ensuring (exists ((atm2: ATMap, qm2: ATMap) => MC(atm1, qm1) |- q2 :: MC(atm2, qm2)))

  @Axiom
  def Tqseq_inv3(atm: ATMap, qm: ATMap, q1: Questionnaire, q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap, atm2: ATMap, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- qseq(q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm1, qm1) |- q2 :: MC(atm2, qm2))
  } ensuring (atmr == atm2)

  @Axiom
  def Tqseq_inv4(atm: ATMap, qm: ATMap, q1: Questionnaire, q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap, atm2: ATMap, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- qseq(q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm1, qm1) |- q2 :: MC(atm2, qm2))
  } ensuring (qmr == qm2)


  @Axiom
  def Tqcond_inv1(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
             q2: Questionnaire, atmr: ATMap, qmr: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
  } ensuring (echeck(atm, exp) == someAType(YesNo()))

  @Axiom
  def Tqcond_inv2(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
  } ensuring (exists ((atm1: ATMap, qm1: ATMap) => MC(atm, qm) |- q1 :: MC(atm1, qm1)))

  @Axiom
  def Tqcond_inv3(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
  } ensuring (exists ((atm1: ATMap, qm1: ATMap) => MC(atm, qm) |- q2 :: MC(atm1, qm1)))


  @Axiom
  def Tqcond_inv4(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap, atm2: ATMap, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm2, qm2))
  } ensuring (atm1 == atm2)

  @Axiom
  def Tqcond_inv5(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap, atm2: ATMap, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm2, qm2))
  } ensuring (qm1 == qm2)

  @Axiom
  def Tqcond_inv6(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm1, qm1))
  } ensuring (atmr == atm1)

  @Axiom
  def Tqcond_inv7(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire,
                  q2: Questionnaire, atmr: ATMap, qmr: ATMap, atm1: ATMap, qm1: ATMap): Unit = {
    require(MC(atm, qm) |- qcond(exp, q1, q2) :: MC(atmr, qmr))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm1, qm1))
  } ensuring (qmr == qm1)



  @Axiom
  def Tqgroup_inv(atm: ATMap, qm: ATMap, q: Questionnaire, atm1: ATMap, qm1: ATMap, gid: GID): Unit = {
    require(MC(atm, qm) |- qgroup(gid, q) :: MC(atm1, qm1))
  } ensuring (MC(atm, qm) |- q :: MC(atm1, qm1))




  //Progress property and necessary auxiliary lemmas
  @Property
  def Progress(am: AnsMap, qm: QMap, q: Questionnaire, atm: ATMap,
      qtm: ATMap, atm2: ATMap, qtm2: ATMap): Unit = {
    require(!isValue(q))
    require(typeAM(am) == atm)
    require(typeQM(qm) == qtm)
    require(MC(atm, qtm) |- q :: MC(atm2, qtm2))
  } ensuring exists((am0: AnsMap, qm0: QMap, q0: Questionnaire) =>
    reduce(q, am, qm) == someQConf(QC(am0, qm0, q0)))

  @Property
  def reduceExpProgress(e: Exp, am: AnsMap, atm: ATMap, at: AType): Unit = {
    require(!expIsValue(e))
    require(typeAM(am) == atm)
    require(echeck(atm, e) == someAType(at))
  } ensuring exists((eres: Exp) => reduceExp(e, am) == someExp(eres))

  @Property
  def lookupAnsMapProgress(am: AnsMap, atm: ATMap, qid: QID, at: AType): Unit = {
    require(typeAM(am) == atm)
    require(lookupATMap(qid, atm) == someAType(at))
  } ensuring exists((av0: Aval) => lookupAnsMap(qid, am) == someAval(av0))

  @Property
  def lookupQMapProgress(qm: QMap, qtm: ATMap, qid: QID, at: AType): Unit = {
    require(typeQM(qm) == qtm)
    require(lookupATMap(qid, qtm) == someAType(at))
  } ensuring exists((qid0: QID, l0: Label, t0: AType) =>
    lookupQMap(qid, qm) == someQuestion(qid0, l0, t0))

  @Property
  def evalBinOpProgress(atm: ATMap, bot: BinOpT, at: AType, a: Aval, a1: Aval): Unit =  {
    require(echeck(atm, binop(constant(a), bot, constant(a1))) == someAType(at))
  } ensuring(exists ((eres: Exp) => evalBinOp(bot, a, a1) == someExp(eres)))

  @Property
  def evalUnOpProgress(atm: ATMap, uot: UnOpT, at: AType, a: Aval, a1: Aval): Unit =  {
    require(echeck(atm, unop(uot, constant(a))) == someAType(at))
  } ensuring(exists ((eres: Exp) => evalUnOp(uot, a) == someExp(eres)))


  //Preservation property and necessary auxiliary lemmas
  @Property
  def Preservation(atm: ATMap, qtm: ATMap, q: Questionnaire, atm1: ATMap, qtm1: ATMap,
                   am: AnsMap, qm: QMap, amr: AnsMap, qmr: QMap, qr: Questionnaire, atmr: ATMap, qtmr: ATMap): Unit = {
    require(MC(atm, qtm) |- q :: MC(atm1, qtm1))
    require(typeAM(am) == atm)
    require(typeQM(qm) == qtm)
    require(reduce(q, am, qm) == someQConf(QC(amr, qmr, qr)))
    require(atmr == typeAM(amr))
    require(qtmr == typeQM(qmr))
  } ensuring (MC(atmr, qtmr) |- qr :: MC(atm1, qtm1))

  @Property
  def reduceExpPreservation(e: Exp, am: AnsMap, atm: ATMap, at: AType, er: Exp): Unit = {
    require(echeck(atm, e) == someAType(at))
    require(typeAM(am) == atm)
    require(reduceExp(e, am) == someExp(er))
  } ensuring(echeck(atm, er) == someAType(at))

  @Property
  def lookupAnsMapPreservation(am: AnsMap, atm: ATMap, qid: QID, at: AType, avr: Aval): Unit = {
    require(lookupATMap(qid, atm) == someAType(at))
    require(lookupAnsMap(qid, am) == someAval(avr))
    require(typeAM(am) == atm)
  } ensuring(typeOf(avr) == at)

  @Property
  def lookupQMapPreservation(qm: QMap, atm: ATMap, qid: QID, at: AType, l: Label, t: AType): Unit = {
    require(lookupATMap(qid, atm) == someAType(at))
    require(lookupQMap(qid, qm) == someQuestion(qid, l, t))
    require(typeQM(qm) == atm)
  } ensuring(at == t)

  @Property
  def evalBinOpPreservation(atm: ATMap, bot: BinOpT, at: AType, a: Aval, a1: Aval, eres: Exp): Unit = {
    require(echeck(atm, binop(constant(a), bot, constant(a1))) == someAType(at))
    require(evalBinOp(bot, a, a1) == someExp(eres))
  } ensuring(echeck(atm, eres) == someAType(at))

  @Property
  def evalUnOpPreservation(uot: UnOpT, av: Aval, atm: ATMap, at: AType, eres: Exp): Unit = {
    require(echeck(atm, unop(uot, constant(av))) == someAType(at))
    require(evalUnOp(uot, av) == someExp(eres))
  } ensuring(echeck(atm, eres) == someAType(at))
}
