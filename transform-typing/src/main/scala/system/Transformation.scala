package system

import system.Syntax._
import system.Verification.Obligation
import veritas.benchmarking.Proved

import scala.collection.immutable.ListMap

abstract class Transformation(val lang: Language) {
  val contracts: ListMap[Rule, Int]
  val rewrites: Seq[Rewrite]

//  final lazy val contractedTerms = contracts.map{case (c,pos) => c.conclusion.terms(pos).asInstanceOf[App]}
  final lazy val contractedSym = rewrites.head.sym


  final lazy val undeclaredSymbols = {
    val csyms = contracts.foldLeft(Set[Symbol]())((set, c) => set ++ c._1.symbols)
    val rsyms = rewrites.foldLeft(Set[Symbol]())((set, r) => set ++ r.symbols)
    val otherTransSyms = lang.transs.map(_.contractedSym).toSet
    (csyms++rsyms).diff(lang.syms.toSet).diff(lang.undeclaredSymbols).diff(otherTransSyms) - contractedSym
  }

  def checkSyntax(): Unit = {
    assert(rewrites.nonEmpty, s"Transformation requires at least one rewrited rule")
    contracts.foreach { case (c, pos) =>
      assert(c.contractedTerm(pos).sym == contractedSym)
      assert(pos < c.conclusion.terms.size)
    }
    rewrites.foreach { r =>
      assert(
        r.pat.isInstanceOf[App] && r.pat.sym == contractedSym,
        s"Rewrite $r does not match contracted symbol $contractedSym")
    }
  }

  private def contractString(contract: Rule, pos: Int) = {
    val premises = contract.premises
    val name = contract.name
    val indent = "  "
    val ps = premises.mkString("\n" + indent)
    s"$name:\n$indent$ps\n$indent=>\n$indent${contract.conclusion.toString(pos)}"
  }

  override def toString: String = {
    val cs = contracts.map(kv => contractString(kv._1, kv._2))

    s"""${contractedSym.sigString}
       |
       |contracts
       |${cs.mkString("\n\n")}
       |
       |rewritings
       |${rewrites.mkString("\n")}
       """.stripMargin
  }

  lazy val soundnessObligations: Seq[Obligation] = Soundness.transSoundness(this)
  lazy val soundnessResults = soundnessObligations.map(Verification.verify(_))
  lazy val isSound = soundnessResults.forall(_.status == Proved)

  def apply(kids: Term*): App = App(contractedSym, kids.toList)
}


