package system

import scala.collection.immutable.ListMap

object Syntax {

  sealed trait ISort {
    val open: Boolean
    val name: String
    override def toString: Error = name
  }

  case class Sort(name: String, open: Boolean = false) extends ISort {
    assert(name != "Prop" && name != "Name", s"Must not construct sort with reserved name Prop or Name, in $name")
  }

  case object Prop extends ISort {
    val open = false
    override val name: String = "Prop"
  }

  case object Name extends ISort {
    val open = true
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
    def isFresh = name.startsWith("fresh")
    def isNotin = name.startsWith("notin")
  }

  def equ(sort: ISort): Symbol = Symbol("eq"+sort.name, in = List(sort, sort), out = Prop)
  def neq(sort: ISort): Symbol = Symbol("neq"+sort.name, in = List(sort, sort), out = Prop)

  type Subst = Map[Var, Term]
  type Error = String

  sealed trait Term {
    val sort: ISort

    def apply(i: Int): Term

    def isGround: Boolean

    def freevars: Set[Var]

    def symbols: Set[Symbol]

    def subst(s: Subst, capturing: Boolean = false): Term

    def occurs(v: Var): Boolean

    def unify(t: Term): Either[Subst, Error] = unify(t, Map())
    def unify(t: Term, s: Subst): Either[Subst, Error]

    def matchAgainst(t: Term): Either[Subst, Error] = matchAgainst(t, Map())
    def matchAgainst(t: Term, s: Subst): Either[Subst, Error]

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

    override def unify(t: Term, s: Subst): Either[Subst, Error] = s.get(this) match{
      case Some(t2) => t2.unify(t, s)
      case None =>
        val tt = t.subst(s)
        if (tt.occurs(this))
          Right(s"Occurs check failed, $this occurs in $tt")
        else {
          val news = Map(this -> tt)
          Left(s.mapValues(_.subst(news)) ++ news)
        }
    }

    override def matchAgainst(t: Term, s: Subst): Either[Subst, Error] = s.get(this) match {
      case Some(t2) => t2.matchAgainst(t, s)
      case None if t == this => Left(s)
      case None => Left(s + (this -> t))
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

    override def freevars: Set[Var] = kids.foldLeft(Set[Var]())((set, t) => set ++ t.freevars)

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

    override def unify(t: Term, s: Subst): Either[Subst, Error] = t match {
      case v: Var => t.unify(this, s)
      case App(`sym`, tkids) =>
        kids.zip(tkids).foldLeft[Either[Subst, Error]](Left(s)){
          case (err@Right(_), _) => err
          case (Left(s), (t1, t2)) => t1.unify(t2, s)
        }
      case _ => Right(s"Could not unify $this with $t")
    }

    override def matchAgainst(t: Term, s: Subst): Either[Subst, Error] = t match {
      case App(`sym`, tkids) =>
        kids.zip(tkids).foldLeft[Either[Subst, Error]](Left(s)){
          case (err@Right(_), _) => err
          case (Left(s), (t1, t2)) => t1.matchAgainst(t2, s)
        }
      case _ => Right(s"Could not match $this against $t")
    }


    override def findAll(p: (Term) => Boolean): Seq[Term] = {
      val me = if (p(this)) Seq(this) else Seq()
      me ++ kids.flatMap(_.findAll(p))
    }
  }
  object App {
    def apply(sym: Symbol, kids: Term*): App = App(sym, kids.toList)
    def apply(trans: Transformation, kids: Term*): App = App(trans.contractedSym, kids.toList)
  }

  case class Judg(sym: Symbol, terms: List[Term]) {
    assert(sym.out == Prop, s"Judgments must use a symbol yielding sort Prop, but got $sym yielding ${sym.out}")
    assert(sym.in.size == terms.size, s"Wrong number of kids for symbol $this")
    assert(sym.in.zip(terms) forall (p => p._1 == p._2.sort), s"Wrong argument type of kids for symbol $this")

    def apply(i: Int) = terms(i)

    def updated(i: Int, t: Term) = Judg(sym, terms.updated(i, t))

    def subst(s: Subst): Judg = {
      val newvars = s.values.foldLeft(Set[Var]())((vars, t) => vars ++ t.freevars)
      val oldvars = freevars -- s.keys
      for (v <- oldvars if newvars.contains(v))
        throw new IllegalArgumentException(s"Substitution $s captures variable $v in $this")
      Judg(sym, terms.map(_.subst(s)))
    }

    def freevars: Set[Var] = terms.foldLeft(Set[Var]())((set, t) => set ++ t.freevars)

    def symbols: Set[Symbol] = terms.foldLeft(Set[Symbol]())((set, t) => set ++ t.symbols) + sym

    override def toString: String = s"$sym(${terms.mkString(", ")})"

    def toString(mark: Int): String = {
      val start = s"$sym("
      val end = ")"
      val mid1 = terms.slice(0, mark).mkString(", ")
      val markterm = terms(mark).toString
      var mid2 = if (mark < terms.size) s" [${markterm.substring(1, markterm.size)}] " else ""
      val mid3 = terms.slice(mark + 1, terms.size).mkString(", ")
      if (mid1.isEmpty)
        mid2 = mid2.substring(1)
      if (mid3.isEmpty)
        mid2 = mid2.substring(0, mid2.size - 1)
      start + mid1 + mid2 + mid3 + end
    }
  }
  object Judg {
    def apply(sym: Symbol, terms: Term*): Judg = Judg(sym, terms.toList)
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

    def subst(s: Subst) = Rule(name, conclusion.subst(s), premises.map(_.subst(s)), lemma)

    def symbols: Set[Symbol] = conclusion.symbols ++ premises.foldLeft(Set[Symbol]())((set, j) => set ++ j.symbols)
  }
  object Rule {
    def apply(name: String, conclusion: Judg, premises: Judg*): Rule = Rule(name, conclusion, premises.toList, false)
  }
  object Lemma {
    def apply(name: String, conclusion: Judg, premises: Judg*): Rule = Rule(name, conclusion, premises.toList, true)
  }

  case class Rewrite(pat: App, gen: Term, where: ListMap[Term, Term] = ListMap()) {
    val boundVars = pat.freevars ++ where.keys.flatMap(_.freevars)
    val usedVars = gen.freevars ++ where.values.flatMap(_.freevars)
    assert(usedVars.subsetOf(boundVars), s"Unbound variables ${usedVars.diff(boundVars)} in rewriting $this")
    where.foreach(kv => assert(kv._1.sort == kv._2.sort, s"Type mismatch in where binding $kv of $this"))

    def sym = pat.sym
    def symbols: Set[Symbol] = {
      var syms = pat.symbols ++ gen.symbols
      where.foreach { case (k,v) =>
        syms ++= k.symbols
        syms ++= v.symbols
      }
      syms
    }

    override def toString: String =
      s"$pat ~> $gen" + (if(where.isEmpty) "" else "\n  where " + where.mkString(",\n        "))
  }
}