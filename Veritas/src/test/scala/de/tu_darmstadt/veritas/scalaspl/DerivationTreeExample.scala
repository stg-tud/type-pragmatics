package de.tu_darmstadt.veritas.scalaspl
import de.tu_darmstadt.veritas.scalaspl.QLSpec._

object DerivationTreeExample {

  val base =  MC(atmempty(), atmempty()) |- qempty() :: MC(atmempty(), atmempty())

  object QCondSimple {
    val goal =
      MC(atmempty(), atmempty()) |-
        qcond(constant(Num(zero())), qempty(), qempty()) ::
          MC(atmempty(), atmempty())

    // constraint
    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    intersectATM(atm1, atm2) == atmempty()
    intersectATM(qm1, qm2) == atmempty()

    // premises
    val qs1Prem = MC(atmempty(), atmempty()) |-
      qempty() ::
        MC(atm1, qm1)
    val qs2Prem = MC(atmempty(), atmempty()) |-
      qempty() ::
        MC(atm2, qm2)

    val echeckPrem =
      someAType(YesNo()) == echeck(atmempty(), constant(Num(zero())))
  }

  object QCondQCond {
    val goal =
MC(atmempty(), atmempty()) |-
  qcond(
    constant(Num(zero())),
    qcond(
      constant(B(yes())),
      qempty(),
      qempty()),
    qempty()) ::
  MC(atmempty(), atmempty())

// constraint
val atm1: ATMap = null
val atm2: ATMap = null
val qm1: ATMap = null
val qm2: ATMap = null
intersectATM(atm1, atm2) == atmempty()
intersectATM(qm1, qm2) == atmempty()

val qs1atm1: ATMap = null
val qs1atm2: ATMap = null
val qs1qm1: ATMap = null
val qs1qm2: ATMap = null
intersectATM(qs1atm1, qs1atm2) == atm1
intersectATM(qs1qm1, qs1qm2) == qm1

    // premises
    val qs1Prem =
      MC(atmempty(), atmempty()) |-
        qcond(constant(B(yes())), qempty(), qempty()) ::
        MC(atm1, qm1)

    val qs1qs1Prem =
      MC(atmempty(), atmempty()) |-
        qempty() ::
      MC(qs1atm1, qs1qm1)

    val qs1qs2Prem =
      MC(atmempty(), atmempty()) |-
        qempty() ::
      MC(qs1atm2, qs1qm2)

    val qs1echeckPrem =
      someAType(YesNo()) == echeck(atmempty(), constant(B(yes())))

    val qs2Prem = MC(atmempty(), atmempty()) |-
      qempty() ::
        MC(atm2, qm2)

    val echeckPrem =
      someAType(YesNo()) == echeck(atmempty(), constant(Num(zero())))
  }

  object QCondQSeq {
    val goal =
      MC(atmempty(), atmempty()) |-
        qcond(
          constant(Num(zero())),
          qseq(qempty(), qempty()),
          qempty()) ::
          MC(atmempty(), atmempty())

    // constraint
    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    intersectATM(atm1, atm2) == atmempty()
    intersectATM(qm1, qm2) == atmempty()

    // premises
    val thenPrem =
      MC(atmempty(), atmempty()) |-
        qseq(qempty(), qempty()) ::
        MC(atm1, qm1)

    val thenatm1: ATMap = null
    val thenatm2: ATMap = null
    val thenqm1: ATMap = null
    val thenqm2: ATMap = null
    appendATMap(thenatm1, thenatm2) == atm1
    appendATMap(thenqm1, thenqm2) == qm1

    val thenqs1Prem =
      MC(atmempty(), atmempty()) |-
        qempty() ::
        MC(thenatm1, thenatm2)

    val thenqs2Prem =
      MC(appendATMap(atmempty(), thenatm1), appendATMap(atmempty(), thenqm1))
        qempty() ::
        MC(thenatm2, thenqm2)

    val elsePrem = MC(atmempty(), atmempty()) |-
      qempty() ::
        MC(atm2, qm2)

    val condPrem =
      someAType(YesNo()) == echeck(atmempty(), constant(Num(zero())))
  }



  // next
  object QSeqSimple {
    val goal =
      MC(atmempty(), atmempty()) |-
        qseq(qempty(), qempty()) ::
          MC(atmempty(), atmempty())

    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    appendATMap(atm1, atm2) == atmempty()
    appendATMap(qm1, qm2) == atmempty()

    val qs1Prem =
      MC(atmempty(), atmempty()) |- qempty() :: MC(atm1, qm1)

    val qs2Prem =
      MC(appendATMap(atmempty(), atm1), appendATMap(atmempty(), qm1)) |-
        qempty() :: MC(atm2, qm2)
  }

  object QSeqQSeq {
    val goal =
      MC(atmempty(), atmempty()) |-
        qseq(qempty(), qseq(qempty(), qempty())) ::
          MC(atmempty(), atmempty())

    // constraints
    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    appendATMap(atm1, atm2) == atmempty()
    appendATMap(qm1, qm2) == atmempty()
    atm2 == appendATMap(atm1next, atm2next)
    qm2 == appendATMap(qm1next, qm2next)
    val atm1next: ATMap = null
    val atm2next: ATMap = null
    val qm1next: ATMap = null
    val qm2next: ATMap = null

    val qs1Prem =
      MC(atmempty(), atmempty()) |- qempty() :: MC(atm1, qm1)

    val qs2Prem =
      MC(appendATMap(atmempty(), atm1), appendATMap(atmempty(), qm1)) |-
        qseq(qempty(), qempty()) :: MC(atm2, qm2)


    val qs2qs1Prem =
      MC(
        appendATMap(appendATMap(atmempty(), atm1), atm1next),
        appendATMap(appendATMap(atmempty(), qm1), qm1next)) |-
          qempty() :: MC(atm2next, qm2next)

    val qs2qs2Prem =
      MC(appendATMap(atmempty(), atm1), appendATMap(atmempty(), qm1)) |-
        qempty() :: MC(atm2, qm2)
  }

  object QSeqQCond {
    val goal =
      MC(atmempty(), atmempty()) |-
        qseq(
          qempty(),
          qcond(constant(B(no())),
            qempty(),
            qempty())) ::
        MC(atmempty(), atmempty())

    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    appendATMap(atm1, atm2) == atmempty()
    appendATMap(qm1, qm2) == atmempty()

    val qs2atm1: ATMap = null
    val qs2atm2: ATMap = null
    val qs2qm1: ATMap = null
    val qs2qm2: ATMap = null
    intersectATM(qs2atm1, qs2atm2) == atm2
    intersectATM(qs2qm1, qs2qm2) == qm2

    val qs1Prem =
      MC(atmempty(), atmempty()) |- qempty() :: MC(atm1, qm1)

    val qs2Prem =
      MC(appendATMap(atmempty(), atm1),
         appendATMap(atmempty(), qm1)) |-
         qcond(
           constant(B(no())),
           qempty(),
           qempty()) ::
         MC(atm2, qm2)

    val qs2thenPrem =
      MC(appendATMap(atmempty(), atm1),
        appendATMap(atmempty(), qm1)) |-
        qempty() ::
          MC(qs2atm1, qs2qm1)

    val qs2eslePrem =
      MC(appendATMap(atmempty(), atm1),
        appendATMap(atmempty(), qm1)) |-
          qempty() ::
          MC(qs2atm2, qs2qm2)

    val qs2CondPrem =
      echeck(appendATMap(atmempty(), atm1), constant(B(no()))) == someAType(YesNo())
  }

  object QSeqAsk {
    val qid = new QID {}
    val goal =
      MC(atmempty(), atmbind(qid, YesNo(), atmempty())) |-
        qseq(qempty(), qsingle(ask(qid))) ::
          MC(atmbind(qid, YesNo(), atmempty()), atmempty())

    val atm1: ATMap = null
    val atm2: ATMap = null
    val qm1: ATMap = null
    val qm2: ATMap = null
    appendATMap(atm1, atm2) == atmbind(qid, YesNo(), atmempty())
    appendATMap(qm1, qm2) == atmempty()

    val qs1Prem =
      MC(atmempty(), atmbind(qid, YesNo(), atmempty())) |- qempty() :: MC(atm1, qm1)

    val qs1lqmPrem =
      lookupATMap(qid, atmbind(qid, YesNo(), atmempty())) == someAType(YesNo())
    val qs1latmPrem =
      lookupATMap(qid, atmempty()) == noAType()

    val qs2Prem =
      MC(appendATMap(atmempty(), atm1), appendATMap(atmempty(), qm1)) |-
        qempty() :: MC(atm2, qm2)
  }
}
