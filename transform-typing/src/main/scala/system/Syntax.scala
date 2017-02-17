package system

import scala.language.implicitConversions

object Syntax {

  implicit def stringOp(s: String) = new StringOp(s)
  class StringOp(val s: String) extends AnyVal {
    def ~(sort: ISort): Var = Var(s, sort)
  }

  sealed trait ISort {
    val abstractEnum: Boolean
    val name: String
    override def toString: Error = name
  }

  case class Sort(name: String, abstractEnum: Boolean = false) extends ISort {
    assert(name != "Prop" && name != "Name", s"Must not construct sort with reserved name Prop or Name, in $name")
  }

  case object Prop extends ISort {
    val abstractEnum = false
    override val name: String = "Prop"
  }

  case object Name extends ISort {
    val abstractEnum = true
    override val name: String = "Name"
  }

  case class Symbol(name: String, in: List[ISort], out: ISort, constr: Boolean = true) {
    override def toString: String = name

    def sigString =
      if (in.isEmpty)
        s"$name: $out"
      else
        s"$name: ${in.mkString(" ")} -> $out"

    def apply(kids: Term*): App = App(this, kids.toList)

    def isEq = name.startsWith("eq")
    def isNeq = name.startsWith("neq")
    def isEqNeq = isEq || isNeq
    def isFresh = name.startsWith("fresh")
    def isNotin = name.startsWith("notin")
    def isExists = name.startsWith("EXISTS")
  }

  def equ(sort: ISort): Symbol = Symbol("eq"+sort.name, in = List(sort, sort), out = Prop)
  def neq(sort: ISort): Symbol = Symbol("neq"+sort.name, in = List(sort, sort), out = Prop)
  val FALSE = Symbol("FALSE", in = List(), out = Prop)
  val TRUE = Symbol("TRUE", in = List(), out = Prop)
  def EXISTS(sort: ISort) = Symbol("EXISTS"+sort.name, in = List(sort, Prop), out = Prop)
  val AND = Symbol("AND", in = List(Prop, Prop), out = Prop)
  val OR = Symbol("OR", in = List(Prop, Prop), out = Prop)
  val XOR = Symbol("XOR", in = List(Prop, Prop), out = Prop)
  val NOT = Symbol("NOT", in = List(Prop), out = Prop)

  def mkExists(vs: Seq[Var], t: Term): Term = vs.foldRight(t)((v, t) => EXISTS(v.sort)(v, t))

  def mkAnd(ps: Seq[Term]): Term =
    if (ps.isEmpty) TRUE()
    else if (ps.tail.isEmpty) ps.head
    else AND(ps.head, mkAnd(ps.tail))

  def mkOr(ps: Seq[Term]): Term =
    if (ps.isEmpty) FALSE()
    else if (ps.tail.isEmpty) ps.head
    else OR(ps.head, mkOr(ps.tail))

  def mkXor(ps: Seq[Term]): Term =
    if (ps.isEmpty) FALSE()
    else if (ps.tail.isEmpty) ps.head
    else XOR(ps.head, mkXor(ps.tail))


  type Subst = Map[Var, Term]
  type Diff = Seq[(Term, Term)]
  type Match = (Subst, Diff, Int) // substitution, list of differences, number of constructor matches
  type Error = String

  def matchProp(m: Match): Match = {
    val (s, eqs, num) = m
    val substedEqs = eqs.map(kv => (kv._1.subst(s, capturing = true), kv._2))
    (s, substedEqs, num)
  }
  def matchDiffMsg(m: Match) = s"Could not match the following pairs\n  ${m._2.mkString("\n  ")}"

  sealed trait Term {
    val sort: ISort

    def apply(i: Int): Term

    def isGround: Boolean

    def freevars: Set[Var]

    def symbols: Set[Symbol]

    def subst(s: Subst, capturing: Boolean = false): Term

    def occurs(v: Var): Boolean

    def unify(t: Term): Match = unify(t, (Map(), Seq(), 0))
    def unify(t: Term, s: Match): Match

    def matchAgainst(t: Term): Match = matchProp(matchAgainst(t, (Map(), Seq(), 0)))
    def matchTerm(t: Term): Match = matchProp(matchAgainst(t, (Map(), Seq(), 0)))
    def matchAgainst(t: Term, m: Match): Match

    def findAll(p: Term => Boolean): Seq[Term]
  }

  case class Var(name: String, sort: ISort) extends Term {
    override def apply(i: Int) = throw new IndexOutOfBoundsException(s"Var term $this has no children")

    override def isGround: Boolean = false

    override def toString: String = "$" + name

    override def freevars: Set[Var] = Set(this)

    override def symbols: Set[Symbol] = Set()

    override def subst(s: Subst, capturing: Boolean = false): Term = s.getOrElse(this, this)

    override def occurs(v: Var): Boolean = this == v

    override def unify(t: Term, m: Match): Match = m._1.get(this) match{
      case Some(t2) => t2.unify(t, m)
      case None =>
        val tt = t.subst(m._1)
        if (tt == this)
          m
        else if (tt.occurs(this))
          throw new MatchError(s"Occurs check failed, $this occurs in $tt")
        else {
          val news = Map(this -> tt)
          (m._1.mapValues(_.subst(news)) ++ news, m._2, m._3)
        }
    }

    override def matchAgainst(t: Term, m: Match): Match = m._1.get(this) match {
      case Some(t2) =>
        if (t == t2)
          m
        else
          (m._1, m._2 :+ (t, t2), m._3)
      case None if t == this => m
      case None => (m._1 + (this -> t), m._2, m._3)
    }

    override def findAll(p: (Term) => Boolean): Seq[Term] = if (p(this)) Seq(this) else Seq()
  }

  case class App(sym: Symbol, kids: List[Term]) extends Term {
    assert(sym.in.size == kids.size, s"Wrong number of kids for symbol $this")
    assert(sym.in.zip(kids) forall (p => p._1 == p._2.sort), s"Wrong argument type of kids for symbol $this")

    override val sort: ISort = sym.out

    override def apply(i: Int) = kids(i)

    override def isGround: Boolean = kids.forall(_.isGround)

    override def toString: String = {
      val ks = kids.mkString(", ")
      s"$sym($ks)"
    }

    override def freevars: Set[Var] =
      if (sym.isExists) kids(1).freevars - kids(0).asInstanceOf[Var]
      else kids.foldLeft(Set[Var]())((set, t) => set ++ t.freevars)

    override def symbols: Set[Symbol] = kids.foldLeft(Set[Symbol]())((set, t) => set ++ t.symbols) + sym

    override def subst(s: Subst, capturing: Boolean = false): Term = {
      if (!capturing) {
        val newvars = s.values.foldLeft(Set[Var]())((vars, t) => vars ++ t.freevars)
        val oldvars = freevars -- s.keys
        for (v <- oldvars if newvars.contains(v))
          throw new IllegalArgumentException(s"Substitution $s captures variable $v in $this")
      }
      App(sym, kids.map(_.subst(s, capturing)))
    }

    override def occurs(v: Var): Boolean = kids.exists(_.occurs(v))

    @inline
    def matchCount = 1

    override def unify(t: Term, m: Match): Match = t match {
      case v: Var => t.unify(this, m)
      case App(`sym`, tkids) if sym.constr =>
        kids.zip(tkids).foldLeft((m._1, m._2, m._3 + matchCount)){
          case (m, (t1, t2)) => t1.unify(t2, m)
        }
      case App(sym2, _) if sym.constr && sym2.constr =>
        throw new MatchError(s"Cannot unify $this with $t")
      case _ => (m._1, m._2 :+ (this, t), m._3)
    }

    override def matchAgainst(t: Term, m: Match): Match = t match {
      case App(`sym`, tkids) =>
        kids.zip(tkids).foldLeft((m._1, m._2, m._3 + matchCount)){
          case (m, (t1, t2)) => t1.matchAgainst(t2, m)
        }
      case App(sym2, _) if sym.constr && sym2.constr =>
        throw new MatchError(s"Cannot match $this against $t")
      case _ => (m._1, m._2 :+ (this, t), m._3)
    }


    override def findAll(p: (Term) => Boolean): Seq[Term] = {
      val me = if (p(this)) Seq(this) else Seq()
      me ++ kids.flatMap(_.findAll(p))
    }

    def toJudg: Judg = Judg(sym, kids)
  }
  object App {
    def apply(sym: Symbol, kids: Term*): App = App(sym, kids.toList)
    def apply(trans: Transformation, kids: Term*): App = App(trans.contractedSym, kids.toList)
    def isFun(t: Term) = t.isInstanceOf[App] && !t.asInstanceOf[App].sym.constr
    def isConstr(t: Term) = t.isInstanceOf[App] && t.asInstanceOf[App].sym.constr
    def is(p: (Symbol, List[Term]) => Boolean)(t: Term) = t match {
      case App(sym, kids) => p(sym, kids)
      case _ => false
    }
  }

  case class Judg(sym: Symbol, terms: List[Term]) {
    assert(sym.out == Prop, s"Judgments must use a symbol yielding sort Prop, but got $sym yielding ${sym.out}")
    assert(sym.in.size == terms.size, s"Wrong number of kids for symbol $this")
    sym.in.zip(terms).foreach { case (paramtype, arg) =>
      assert(paramtype == arg.sort, s"Wrong argument type ${arg.sort} of $arg in $this")
    }

    def apply(i: Int) = terms(i)

    def updated(i: Int, t: Term) = Judg(sym, terms.updated(i, t))

    def subst(s: Subst, capturing: Boolean = false): Judg = {
      if (!capturing) {
        val newvars = s.values.foldLeft(Set[Var]())((vars, t) => vars ++ t.freevars)
        val oldvars = freevars -- s.keys
        for (v <- oldvars if newvars.contains(v))
          throw new IllegalArgumentException(s"Substitution $s captures variable $v in $this")
      }
      Judg(sym, terms.map(_.subst(s, capturing)))
    }

    def freevars: Set[Var] =
      if (sym.isExists) terms(1).freevars - terms(0).asInstanceOf[Var]
      else terms.foldLeft(Set[Var]())((set, t) => set ++ t.freevars)

    def symbols: Set[Symbol] = terms.foldLeft(Set[Symbol]())((set, t) => set ++ t.symbols) + sym

    def matchTerm(t: Judg): Match = matchProp(matchAgainst(t, (Map(), Seq(), 0)))
    def matchAgainst(j: Judg, m: Match): Match = j match {
      case Judg(`sym`, jterms) =>
        terms.zip(jterms).foldLeft[Match](m){
          case (m, (t1, t2)) => t1.matchAgainst(t2, m)
        }
      case _ => throw new MatchError(s"Cannot match $this against $j")
    }

    override def toString: String = s"$sym(${terms.mkString(", ")})"

    def toString(mark: Int): String = {
      val start = s"$sym("
      val end = ")"
      val mid1 = terms.slice(0, mark).mkString(", ")
      val markterm = terms(mark).toString
      var mid2 = s", [$markterm], "
      val mid3 = terms.slice(mark + 1, terms.size).mkString(", ")
      if (mid1.isEmpty)
        mid2 = mid2.substring(2)
      if (mid3.isEmpty)
        mid2 = mid2.substring(0, mid2.size - 2)
      start + mid1 + mid2 + mid3 + end
    }

    def constrs: Seq[Symbol] = terms.flatMap(_.findAll(t => App.isConstr(t))).map(_.asInstanceOf[App].sym)

    def toApp: App = App(sym, terms)
  }
  object Judg {
    def apply(sym: Symbol, terms: Term*): Judg = new Judg(sym, terms.toList)
  }

  case class Rule(name: String, conclusion: Judg, premises: List[Judg], lemma: Boolean = false) {
    override def toString: String = {
      val indent = "  "
      val ps = premises.mkString("\n" + indent)
      val psn = if (ps.isEmpty) "" else "\n"
      s"$name:\n$indent$ps$psn$indent=>\n$indent$conclusion"
    }

    def contractedTerm(pos: Int) = conclusion.terms(pos).asInstanceOf[App]

    def freevars: Set[Var] = conclusion.freevars ++ premises.foldLeft(Set[Var]())((set, j) => set ++ j.freevars)

    def fresh(implicit gensym: Gensym): Rule = {
      val sfresh = freevars.map(v => v -> gensym.freshVar(v.name, v.sort)).toMap
      this.subst(sfresh)
    }

    def subst(s: Subst, capturing: Boolean = false) =
      Rule(name, conclusion.subst(s, capturing), premises.map(_.subst(s, capturing)), lemma)

    def symbols: Set[Symbol] = conclusion.symbols ++ premises.foldLeft(Set[Symbol]())((set, j) => set ++ j.symbols)
  }
  object Rule {
    def apply(name: String, conclusion: Judg, premises: Judg*): Rule = Rule(name, conclusion, premises.toList, false)
  }
  object Lemma {
    def apply(name: String, conclusion: Judg, premises: Judg*): Rule = Rule(name, conclusion, premises.toList, true)
  }

  case class Rewrite(pat: App, gen: Term, where: Seq[Judg] = Seq()) {
    assert(pat.sort == gen.sort)
    val boundVars = pat.freevars ++ where.flatMap(_.freevars)
    val usedVars = gen.freevars // ++ where.values.flatMap(_.freevars)
    assert(usedVars.subsetOf(boundVars), s"Unbound variables ${usedVars.diff(boundVars)} in rewriting $this")

    def sym = pat.sym
    def symbols: Set[Symbol] = pat.symbols ++ gen.symbols ++ where.flatMap(_.symbols)

    override def toString: String =
      s"$pat ~> $gen" + (if(where.isEmpty) "" else "\n  where " + where.mkString(",\n        "))
  }
}