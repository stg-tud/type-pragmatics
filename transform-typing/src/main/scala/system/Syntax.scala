package system

object Syntax {

  trait ISort

  case class Sort(name: String) extends ISort {
    assert(name != "Prop")
    override def toString: String = name
  }

  case object Prop extends ISort {
    override def toString: String = "Prop"
  }

  case class Symbol(name: String, in: List[ISort], out: ISort) {
    override def toString: String = name
    def sigString =
      if (in.isEmpty)
        s"$name: $out"
      else
        s"$name: ${in.mkString(" ")} -> $out"
  }

  trait Term {
    val sort: ISort

    def apply(i: Int): Term

    def isGround: Boolean

    def freevars: Set[Var]
  }

  case class Var(name: String, sort: ISort) extends Term {
    override def apply(i: Int) = throw new IndexOutOfBoundsException(s"Var term $this has no children")

    override def isGround: Boolean = false

    override def toString: String = "$" + name

    override def freevars: Set[Var] = Set(this)
  }

  case class App(sym: Symbol, kids: List[Term]) extends Term {
    assert(sym.in.size == kids.size)
    assert(sym.in.zip(kids) forall (p => p._1 == p._2.sort))

    override val sort: ISort = sym.out

    override def apply(i: Int) = kids(i)

    override def isGround: Boolean = kids.forall(_.isGround)

    override def toString: String = s"($sym ${kids.mkString(" ")})"

    override def freevars: Set[Var] = kids.toSet.flatMap(_.freevars)
  }

  case class Judg(name: String, terms: List[Term]) {
    def apply(i: Int) = terms(i)

    override def toString: String = s"$name(${terms.mkString(" ")})"

    def toString(mark: Int): String = {
      val start = s"$name("
      val end = ")"
      val mid1 = terms.slice(0, mark).mkString(" ")
      val mid2 = if (mark < terms.size) s"[${terms(mark)}]" else ""
      val mid3 = terms.slice(mark + 1, terms.size).mkString(" ")
      start + mid1 + mid2 + mid3 + end
    }
  }

  case class Rule(name: String, conclusion: Judg, premises: List[Judg]) {
    override def toString: String = {
      val indent = "  "
      val ps = premises.mkString("\n" + indent)
      s"$name:\n$indent$ps\n$indent=>\n$indent$conclusion"
    }
  }

  case class Rewrite(pat: Term, gen: Term) {
    assert(gen.freevars.subsetOf(pat.freevars))
    override def toString: String = s"$pat ~> $gen"
  }

  case class Transform(contract: Rule, pos: Int, rewrites: List[Rewrite]) {
    assert(pos < contract.conclusion.terms.size)
    assert(contract.conclusion.terms(pos).isInstanceOf[App])

    val contractedSym = contract.conclusion.terms(pos).asInstanceOf[App].sym
    rewrites.foreach(r =>
      assert(r.pat.isInstanceOf[App] && r.pat.asInstanceOf[App].sym == contractedSym, s"Rewrite $r does not match contracted symbol $contractedSym"))

    override def toString: String = {
      val premises = contract.premises
      val name = contract.name
      val indent = "  "
      val ps = premises.mkString("\n" + indent)
      val scontract = s"$name:\n$indent$ps\n$indent=>\n$indent${contract.conclusion.toString(pos)}"
      s"""${contractedSym.sigString}
         |
         |contract
         |$scontract
         |
         |rewritings
         |${rewrites.mkString("\n")}}
       """.stripMargin
    }
  }

}