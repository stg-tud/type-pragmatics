package de.tu_darmstadt.veritas.newinputdsl

object QLSpec extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true
  override def typable(exp: Expression, typ: Typ) = true

  // BasicTypes
  trait YN extends Expression
  case class yes() extends YN
  case class no() extends YN

  def and(a: YN, b: YN): YN = (a, b) match {
    case (yes(), yes()) => yes()
    case (b1, b2) => no()
  }

  def or(a: YN, b: YN): YN = (a, b) match {
    case (no(), no()) => no()
    case (b1, b2) => yes()
  }

  def not(a: YN): YN = a match {
    case yes() => no()
    case no() => yes()
  }

  trait nat extends Expression
  case class zero() extends nat
  case class succ(n: nat) extends nat

  def pred(n: nat): nat = n match {
    case zero() => zero()
    case succ(n) => n
  }

  def gt(a: nat, b: nat): YN = (a, b) match {
    case (zero(), n) => no()
    case (succ(n), zero()) => yes()
    case (succ(n1), succ(n2)) => gt(n1, n1)
  }

  def lt(a: nat, b: nat): YN = (a, b) match {
    case (n, zero()) => no()
    case (zero(), succ(n)) => yes()
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
    case (n, zero()) => zero()
    case (n1, succ(n2)) => plus(n1, multiply(n1, n2))
  }

  def divide(a: nat, b: nat): nat = (a, b) match {
    case (n1, n2) =>
      if(gt(n1, n2) == yes())
        succ(divide(minus(n1, n2), n2))
      else
        zero()
  }

  @Open
  trait char extends Expression

  trait string extends Expression
  case class sempty() extends string
  case class scons(c: char, tail: string) extends string

  // QLSyntax
  @Open
  trait QID extends Expression

  @Open
  trait GID extends Expression

  @Open
  trait Label extends Expression

  trait Aval extends Expression
  case class B(value: YN) extends Aval
  case class Num(value: nat) extends Aval
  case class T(value: string) extends Aval

  trait OptAval extends Expression
  case class noAval() extends OptAval
  case class someAval(value: Aval) extends OptAval

  def isSomeAval(opt: OptAval): Boolean = opt match {
    case noAval() => false
    case someAval(aval) => true
  }

  // TODO mark as partial
  def getAval(opt: OptAval): Aval = opt match {
    case someAval(aval) => aval
  }

  trait AType extends Expression
  case class YesNo() extends AType
  case class Number() extends AType
  case class Text() extends AType

  trait OptAType extends Expression
  case class noAType() extends OptAType
  case class someAType(typ: AType) extends OptAType

  def isSomeAType(opt: OptAType): Boolean = opt match {
    case noAType() => false
    case someAType(atype) => true
  }

  // TODO mark as partial
  def getAType(opt: OptAType): AType = opt match {
    case someAType(atype) => atype
  }

  def typeOf(aval: Aval): AType = aval match {
    case B(yn) => YesNo()
    case Num(n) => Number()
    case T(s) => Text()
  }

  trait ATList extends Expression
  case class atempty() extends ATList
  case class atcons(atype: AType, rem: ATList) extends ATList

  def append(atl1: ATList, atl2: ATList): ATList = (atl1, atl2) match {
    case (atempty(), atl) => atl
    case (atcons(atype, atlr), atl) => atcons(atype, append(atlr, atl))
  }

  trait BinOp extends Expression
  case class addop() extends BinOp
  case class subop() extends BinOp
  case class mulop() extends BinOp
  case class divop() extends BinOp
  case class eqop() extends BinOp
  case class gtop() extends BinOp
  case class ltop() extends BinOp
  case class andop() extends BinOp
  case class orop() extends BinOp

  trait UnOp extends Expression
  case class notop() extends UnOp

  trait Exp extends Expression
  case class constant(aval: Aval) extends Exp
  case class qvar(qid: QID) extends Exp
  case class binop(e1: Exp, op: BinOp, e2: Exp) extends Exp
  case class unop(op: UnOp, e: Exp) extends Exp

  trait Entry extends Expression
  case class question(qid: QID, l: Label, at: AType) extends Entry
  case class value(qid: QID, at: AType, exp: Exp) extends Entry
  case class defquestion(qid: QID, l: Label, at: AType) extends Entry
  case class ask(qid: QID) extends Entry

  trait Questionnaire extends Expression
  case class qempty() extends Questionnaire
  case class qsingle(entry: Entry) extends Questionnaire
  case class qseq(qs1: Questionnaire, qs2: Questionnaire) extends Questionnaire
  case class qcond(e: Exp, els: Questionnaire, thn: Questionnaire) extends Questionnaire
  case class qgroup(gid: GID, qs: Questionnaire) extends Questionnaire

  // QLSemanticsData

  trait AnsMap extends Expression
  case class aempty() extends AnsMap
  case class abind(qid: QID, aval: Aval, al: AnsMap) extends AnsMap

  def lookupAnsMap(qid: QID, am: AnsMap): OptAval = (qid, am) match {
    case (qid, aempty()) => noAval()
    case (qid1, abind(qid2, aval, aml)) =>
      if (qid1 == qid2)
        someAval(aval)
      else
        lookupAnsMap(qid1, aml)
  }

  def appendAnsMap(am1: AnsMap, am2: AnsMap): AnsMap = (am1, am2) match {
    case (aempty(), am) => am
    case (abind(qid, av, am), aml) => abind(qid, av, appendAnsMap(am, aml))
  }

  trait QMap extends Expression
  case class qmempty() extends QMap
  case class qmbind(qid: QID, l: Label, atype: AType, qml: QMap) extends QMap

  trait OptQuestion extends Expression
  case class noQuestion() extends OptQuestion
  case class someQuestion(qid: QID, l: Label, atype: AType) extends OptQuestion

  def isSomeQuestion(opt: OptQuestion): Boolean = opt match {
    case noQuestion() => false
    case someQuestion(qid, l, at) => true
  }

  // TODO mark as partial
  def getQuestionQID(opt: OptQuestion): QID = opt match {
    case someQuestion(qid, l, at) => qid
  }

  // TODO mark as partial
  def getQuestionLabel(opt: OptQuestion): Label = opt match {
    case someQuestion(qid, l, at) => l
  }

  // TODO mark as partial
  def getQuestionAType(opt: OptQuestion): AType = opt match {
    case someQuestion(qid, l, at) => at
  }

  def lookupQMap(qid: QID, qm: QMap): OptQuestion = (qid, qm) match {
    case (qid, qmempty()) => noQuestion()
    case (qid1, qmbind(qid2, l, at, qml)) =>
      if (qid1 == qid2)
        someQuestion(qid, l, at)
      else
        lookupQMap(qid1, qml)
  }

  def appendQMap(qm1: QMap, qm2: QMap): QMap = (qm1, qm2) match {
    case (qmempty(), qm) => qm
    case (qmbind(qid, l, at, qm), qml) => qmbind(qid, l, at, appendQMap(qm, qml))
  }

  trait QConf extends Expression
  case class QC(am: AnsMap, qm: QMap, q: Questionnaire) extends QConf

  def getAM(qc: QConf): AnsMap = qc match {
    case QC(am, qm, q) => am
  }

  def getQM(qc: QConf): QMap = qc match {
    case QC(am, qm, q) => qm
  }

  def getQuest(qc: QConf): Questionnaire = qc match {
    case QC(am, qm, q) => q
  }

  def isValue(qc: QConf): Boolean = qc match {
    case QC(am, qm, qempty()) => true
    case QC(am, qm, q) => false
  }

  trait OptQConf extends Expression
  case class noQConf() extends OptQConf
  case class someQConf(qc: QConf) extends OptQConf

  def isSomeQC(opt: OptQConf): Boolean = opt match {
    case noQConf() => false
    case someQConf(qc) => true
  }

  // TODO mark as partial
  def getQC(opt: OptQConf): QConf = opt match {
    case someQConf(qc) => qc
  }

  def qcappend(qc: QConf, q: Questionnaire): QConf = (qc, q) match {
    case (QC(am, qm, qs1), qs2) => QC(am, qm, qseq(qs1, qs2))
  }

  trait OptExp extends Expression
  case class noExp() extends OptExp
  case class someExp(exp: Exp) extends OptExp

  def isSomeExp(opt: OptExp): Boolean = opt match {
    case noExp() => false
    case someExp(e) => true
  }

  // TODO mark as partial
  def getExp(opt: OptExp): Exp = opt match {
    case someExp(e) => e
  }

  def expIsValue(exp: Exp): Boolean = exp match {
    case constant(av) => true
    case e => false
  }

  // TODO mark as partial
  def getExpValue(exp: Exp): Aval = exp match {
    case constant(av) => av
  }

  // QLSemantics

  // TODO how to design functions that have no body?
  def askYesNo(l: Label): YN = l match {
    case l => yes()
  }
  def askNumber(l: Label): nat = l match {
    case l => zero()
  }
  def askText(l: Label): string = l match {
    case l => sempty()
  }

  def getAnswer(l: Label, at: AType): Aval = (l, at) match {
    case (l, YesNo()) => B(askYesNo(l))
    case (l, Number()) => Num(askNumber(l))
    case (l, Text()) => T(askText(l))
  }

  def evalBinOp(op: BinOp, av1: Aval, av2: Aval): OptExp = (op, av1, av2) match {
    case (addop(), Num(n1), Num(n2)) => someExp(constant(Num(plus(n1, n2))))
    case (subop(), Num(n1), Num(n2)) => someExp(constant(Num(minus(n1, n2))))
    case (mulop(), Num(n1), Num(n2)) => someExp(constant(Num(multiply(n1, n2))))
    case (divop(), Num(n1), Num(n2)) => someExp(constant(Num(divide(n1, n2))))
    case (gtop(), Num(n1), Num(n2)) => someExp(constant(B(gt(n1, n2))))
    case (ltop(), Num(n1), Num(n2)) => someExp(constant(B(lt(n1, n2))))
    case (andop(), B(b1), B(b2)) => someExp(constant(B(and(b1, b2))))
    case (orop(), B(b1), B(b2)) => someExp(constant(B(or(b1, b2))))
    case (eqop(), Num(n1), Num(n2)) =>
      if(n1 == n2)
        someExp(constant(B(yes())))
      else
        someExp(constant(B(no())))
    case (eqop(), B(b1), B(b2)) =>
      if(b1 == b2)
        someExp(constant(B(yes())))
      else
        someExp(constant(B(no())))
    case (eqop(), T(t1), T(t2)) =>
      if(t1 == t2)
        someExp(constant(B(yes())))
      else
        someExp(constant(B(no())))
    case (op, a1, a2) => noExp()
  }

  def evalUnOp(op: UnOp, av: Aval): OptExp = (op, av) match {
    case (notop(), B(b)) => someExp(constant(B(not(b))))
    case (op, a) => noExp()
  }

  def reduceExp(exp: Exp, am: AnsMap): OptExp = (exp, am) match {
    case (constant(av), am) => noExp()
    case (qvar(qid), am) =>
      val avOpt = lookupAnsMap(qid, am)
      if (isSomeAval(avOpt))
        someExp(constant(getAval(avOpt)))
      else
        noExp()
    case (binop(e1, op, e2), am) =>
      if (expIsValue(e1) && expIsValue(e2))
        evalBinOp(op, getExpValue(e1), getExpValue(e2))
      else {
        val eOpt1 = reduceExp(e1, am)
        if (isSomeExp(eOpt1))
          someExp(binop(getExp(eOpt1), op, e2))
        else {
          val eOpt2 = reduceExp(e2, am)
          if (isSomeExp(eOpt2))
            someExp(binop(e1, op, getExp(eOpt2)))
          else noExp()
        }
      }
    case (unop(op, e), am) =>
      if (expIsValue(e))
        evalUnOp(op, getExpValue(e))
      else {
        val eOpt = reduceExp(e, am)
        if (isSomeExp(eOpt))
          someExp(unop(op, getExp(eOpt)))
        else noExp()
      }
  }

  def reduce(qc: QConf): OptQConf = qc match {
    case (QC(am, qm, qempty())) => noQConf()
    case (QC(am, qm, qsingle(question(qid, l, t)))) =>
      val av = getAnswer(l, t)
      someQConf(QC(abind(qid, av, am), qm, qempty()))
    case (QC(am, qm, qsingle(value(qid, t, e)))) =>
      if (expIsValue(e))
        someQConf(QC(abind(qid, getExpValue(e), am), qm, qempty()))
      else {
        val eOpt = reduceExp(e, am)
        if (isSomeExp(eOpt))
          someQConf(QC(am, qm, qsingle(value(qid, t, getExp(eOpt)))))
        else noQConf()
      }
    case (QC(am, qm, qsingle(defquestion(qid, l, t)))) => someQConf(QC(am, qmbind(qid, l, t, qm), qempty()))
    case (QC(am, qm, qsingle(ask(qid)))) =>
      val qOpt = lookupQMap(qid, qm)
      if (isSomeQuestion(qOpt))
        someQConf(QC(am, qm, qsingle(question(qid, getQuestionLabel(qOpt), getQuestionAType(qOpt)))))
      else noQConf()
    case (QC(am, qm, qseq(qempty(), qs))) => someQConf(QC(am, qm, qs))
    case (QC(am, qm, qseq(qs1, qs2))) =>
      val qcOpt = reduce(QC(am, qm, qs1))
      if (isSomeQC(qcOpt))
        someQConf(qcappend(getQC(qcOpt), qs2))
      else noQConf()
    case (QC(am, qm, qcond(constant(B(yes())), qs1, qs2))) => someQConf(QC(am, qm, qs1))
    case (QC(am, qm, qcond(constant(B(no())), qs1, qs2))) => someQConf(QC(am, qm, qs2))
    case (QC(am, qm, qcond(e, qs1, qs2))) =>
      val eOpt = reduceExp(e, am)
      if (isSomeExp(eOpt))
        someQConf(QC(am, qm, qcond(getExp(eOpt), qs1, qs2)))
      else noQConf()
    case (QC(am, qm, qgroup(gid, qs))) => someQConf(QC(am, qm, qs))
  }

  // QLTypeSystem

  trait ATMap extends Context with Typ
  case class atmempty() extends ATMap
  case class atmbind(qid: QID, at: AType, atml: ATMap) extends ATMap

  def lookupATMap(qid: QID, atm: ATMap): OptAType = (qid, atm) match {
    case (qid, atmempty()) => noAType()
    case (qid1, atmbind(qid2, at, atml)) =>
      if (qid1 == qid2) someAType(at)
      else lookupATMap(qid1, atml)
  }

  def appendATMap(atm1: ATMap, atm2: ATMap): ATMap = (atm1, atm2) match {
    case (atmempty(), atm) => atm
    case (atmbind(qid, at, atm), atml) => atmbind(qid, at, appendATMap(atm, atml))
  }

  def intersectATM(atm1: ATMap, atm2: ATMap): ATMap = (atm1, atm2) match {
    case (atmempty(), atm) => atmempty()
    case (atm, atmempty()) => atmempty()
    case (atmbind(qid, at, atm1), atm2) =>
      val atm1atm2 = intersectATM(atm1, atm2)
      val lAT = lookupATMap(qid, atm2)
      if (isSomeAType(lAT) && getAType(lAT) == at)
        atmbind(qid, at, atm1atm2)
      else
        atm1atm2
  }

  trait MapConf extends Context with Typ
  case class MC(atm: ATMap, qtm: ATMap) extends MapConf

  trait OptMapConf extends Context with Typ
  case class noMapConf() extends OptMapConf
  case class someMapConf(mc: MapConf) extends OptMapConf

  def isSomeMapConf(opt: OptMapConf): Boolean = opt match {
    case noMapConf() => false
    case someMapConf(mc) => true
  }

  // TODO mark as partial
  def getMapConf(opt: OptMapConf): MapConf = opt match {
    case someMapConf(mc) => mc
  }

  def typeAM(am: AnsMap): ATMap = am match {
    case aempty() => atmempty()
    case abind(qid, av, amr) => atmbind(qid, typeOf(av), typeAM(amr))
  }

  def typeQM(qm: QMap): ATMap = qm match {
    case qmempty() => atmempty()
    case qmbind(qid, l, at, qmr) => atmbind(qid, at, typeQM(qmr))
  }

  def checkBinOp(op: BinOp, at1: AType, at2: AType): OptAType = (op, at1, at2) match {
    case (addop(), Number(),  Number()) => someAType(Number())
    case (subop(), Number(),  Number()) => someAType(Number())
    case (mulop(), Number(),  Number()) => someAType(Number())
    case (divop(), Number(),  Number()) => someAType(Number())
    case (gtop(), Number(),  Number()) => someAType(YesNo())
    case (ltop(), Number(),  Number()) => someAType(YesNo())
    case (andop(), YesNo(),  YesNo()) => someAType(YesNo())
    case (orop(), YesNo(),  YesNo()) => someAType(YesNo())
    case (eqop(), Number(),  Number()) => someAType(YesNo())
    case (eqop(), YesNo(),  YesNo()) => someAType(YesNo())
    case (eqop(), Text(),  Text()) => someAType(YesNo())
    case (op, t1, t2) => noAType()
  }

  def checkUnOp(op: UnOp, at: AType): OptAType = (op, at) match {
    case (notop(), YesNo()) => someAType(YesNo())
    case (op, a) => noAType()
  }

  def echeck(atm: ATMap, exp: Exp): OptAType = (atm, exp) match {
    case (atm, constant(B(n))) => someAType(YesNo())
    case (atm, constant(Num(n))) => someAType(Number())
    case (atm, constant(T(n))) => someAType(Text())
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
  def Tqempty(mc: MapConf): Unit = {
  } ensuring mc |- qempty() :: MC(atmempty(), atmempty())

  @Axiom
  def Tquestion(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType): Unit = {
    require(lookupATMap(qid, atm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(question(qid, l, at)) :: MC(atmbind(qid, at, atmempty()), atmempty())

  @Axiom
  def Tvalue(qid: QID, atm: ATMap, exp: Exp, qm: ATMap, at: AType): Unit = {
    require(lookupATMap(qid, atm) == noAType())
    require(echeck(atm, exp) == someAType(at))
  } ensuring MC(atm, qm) |- qsingle(value(qid, at, exp)) :: MC(atmbind(qid, at, atmempty()), atmempty())

  @Axiom
  def Tdefquestion(qid: QID, atm: ATMap, qm: ATMap, l: Label, at: AType): Unit = {
    require(lookupATMap(qid, qm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(defquestion(qid, l, at)) :: MC(atmempty(), atmbind(qid, at, atmempty()))

  @Axiom
  def Task(qid: QID, qm: ATMap, at: AType, atm: ATMap): Unit = {
    require(lookupATMap(qid, qm) == someAType(at))
    require(lookupATMap(qid, atm) == noAType())
  } ensuring MC(atm, qm) |- qsingle(ask(qid)) :: MC(atmbind(qid, at, atmempty()), atmempty())

  @Axiom
  def Tqseq(atm: ATMap, qm: ATMap, q1: Questionnaire, atm1: ATMap, atm2: ATMap, qm1: ATMap, q2: Questionnaire, qm2: ATMap): Unit = {
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(appendATMap(atm, atm1), appendATMap(qm, qm1)) |- q2 :: MC(atm2, qm2))
  } ensuring MC(atm, qm) |- qseq(q1, q2) :: MC(appendATMap(atm1, atm2), appendATMap(qm1, qm2))

  @Axiom
  def Tqcond(atm: ATMap, exp: Exp, qm: ATMap, q1: Questionnaire, atm1: ATMap, qm1: ATMap, q2: Questionnaire, atm2: ATMap, qm2: ATMap): Unit = {
    require(echeck(atm, exp) == someAType(YesNo()))
    require(MC(atm, qm) |- q1 :: MC(atm1, qm1))
    require(MC(atm, qm) |- q2 :: MC(atm2, qm2))
  } ensuring MC(atm, qm) |- qcond(exp, q1, q2) :: MC(intersectATM(atm1, atm2), intersectATM(qm1, qm2))

  // TODO undefined functions how?
  def qcCheck(mc: MapConf, qc: QConf, atm: ATMap): Boolean = (mc, qc, atm) match {
    case (mc, qc, atm) => true
  }

  @Axiom
  def TqcCheck(am: AnsMap, atm1: ATMap, atm0: ATMap, qm0: ATMap, qm: QMap, q: Questionnaire, atm2: ATMap, qm2: ATMap): Unit = {
    require(typeAM(am) == atm1)
    require(MC(appendATMap(atm0, atm1), appendATMap(qm0, typeQM(qm))) |- q :: MC(atm2 ,qm2))
  } ensuring qcCheck(MC(atm0, qm0), QC(am, qm, q), appendATMap(atm1, atm2))

}
