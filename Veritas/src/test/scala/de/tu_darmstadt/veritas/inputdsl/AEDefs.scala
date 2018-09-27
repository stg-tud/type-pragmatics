package de.tu_darmstadt.veritas.inputdsl

object AEDefs {
  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._

  //simple Boolean and arithmetic expressions
  val term = data('Term) of
    'true | 'false | 'ifelse('Term, 'Term, 'Term)
    'zero | 'succ('Term) | 'pred('Term) | 'iszero('Term)

  val isNV = function('isNV.>>('Term) -> 'Bool) where
    (('isNV('zero) := true) |
      ('isNV('succ('nv)) := 'isNV('nv)) |
      ('isNV('t) := false))

  val isvalue = function('isValue.>>('Term) -> 'Bool) where
    ('isValue('true) := true) |
      ('isValue('false) := true) |
      ('isValue('t) := 'isNV('t))


  val optTerm = data('OptTerm) of
    'noTerm | 'someTerm('Term)

  val isSomeTerm = function('isSomeTerm.>>('OptTerm) -> 'Bool) where
    ('isSomeTerm('noTerm) := false) |
      ('isSomeTerm('someTerm('t)) := true)

  val getTerm = partial(function('getTerm.>>('OptTerm) -> 'Term) where
    ('getTerm('someTerm('t)) := 't))

  //reduction semantics for simple Boolean and arithmetic terms
  val reduce = function('reduce.>>('Term) -> 'OptTerm) where
    ('reduce('ifelse('true, 't2, 't3)) := 'someTerm('t2)) |
      ('reduce('ifelse('false, 't2, 't3)) := 'someTerm('t3)) |
      ('reduce('ifelse('t1, 't2, 't3)) := ((let ('ot1) := 'reduce('t1)) in
        (iff ('isSomeTerm('ot1))
          th 'someTerm('ifelse('getTerm('ot1), 't2, 't3))
          els 'noTerm))) |
    ('reduce('succ('t1)) := ((let('ot2) := 'reduce('t1)) in
      (iff ('isSomeTerm('ot2))
        th 'someTerm('succ('getTerm('ot2)))
        els 'noTerm))) |
      ('reduce('pred('zero)) := 'someTerm('zero)) |
      ('reduce('pred('succ('nv))) := iff ('isNV('nv)) th 'someTerm('nv) els 'noTerm) |
      ('reduce('pred('t1)) := ((let ('ot2) := 'reduce('t1)) in
        (iff ('isSomeTerm('ot2))
          th 'someTerm('pred('getTerm('ot2)))
          els 'noTerm))) |
      ('reduce('iszero('zero)) := 'someTerm('true)) |
      ('reduce('iszero('succ('nv))) := iff ('isNV('nv)) th 'someTerm('false) els 'noTerm) |
      ('reduce('iszero('t1)) := ((let ('ot2) := 'reduce('ot2)) in
        (iff ('isSomeTerm('ot2))
          th 'someTerm('iszero('getTerm('ot2)))
          els 'noTerm)))

  //types (Bool and Nat)
  val types = data('Ty) of 'B | 'Nat


  //typing rules
  val Ttrue = axiom(
    ===>("T-true")(
    'true :: ~'B))

  val Tfalse = axiom(
    ===>("T-false")(
      'false :: ~'B
    ))

  val Tif = axiom(
    ((~'t1 :: 'B) &
      (~'t2 :: ~'T) &
      (~'t3 :: ~'T)
      ).===>("T-if")
      ('ifelse(~'t1, ~'t2, ~'t3) :: ~'T))

  val TNat = axiom(
    ===>("T-Zero")
    ('zero :: 'Nat))

  val TSucc = axiom(
    (~'t1 :: 'Nat
      ).===>("T-Succ")
    ('succ(~'t1) :: 'Nat))

  val TPred = axiom(
    (~'t1 :: 'Nat
      ).===>("T-Pred")
    ('pred(~'t1) :: 'Nat))

  val Tiszero = axiom(
    (~'t1 :: 'Nat
      ).===>("T-Iszero")
    ('iszero(~'t1) :: 'B))

  val progress = goal(
    ((~'t1 :: ~'T) &
      (!('isValue(~'t1)))
      ).===>("Progress")(
      (exists(~'t1) | ('reduce(~'t1) === ~'t2)
    )))

  val preservation = goal(
    ((~'t1 :: ~'T) &
      ('reduce('t1) === 'someTerm('t2))
      ).===>("Preservation")(
      (~'t2 :: ~'T)
    ))


}
