package system

import system.Syntax._
import system.Verification.ProofObligation
import veritas.benchmarking.Proved

import scala.collection.immutable.ListMap

abstract class Transformation(val lang: Language) {
  val contract: (Rule, Int)
  val lemmas: ListMap[Rule, Int] = ListMap()
  val rewrites: Seq[Rewrite]

  lazy val rules: ListMap[Rule, Int] = ListMap(contract) ++ lemmas

//  final lazy val contractedTerms = contracts.map{case (c,pos) => c.conclusion.terms(pos).asInstanceOf[App]}
  final lazy val contractedSym = rewrites.head.sym


  final lazy val undeclaredSymbols = {
    val lsyms = rules.foldLeft(Set[Symbol]())((set, c) => set ++ c._1.symbols)
    val rsyms = rewrites.foldLeft(Set[Symbol]())((set, r) => set ++ r.symbols)
    val otherTransSyms = lang.transs.map(_.contractedSym).toSet
    (lsyms++rsyms).diff(lang.syms.toSet).diff(lang.undeclaredSymbols).diff(otherTransSyms) - contractedSym
  }

  def checkSyntax(): Unit = {
    rules.foreach(kv => assert(!contractedSym.constr, s"Transformation symbol must not be marked as constructor"))
    rules.foreach(kv => assert(kv._1.lemma, s"Transformation contracts must be marked as lemmas"))
    rules.foreach { case (c, pos) =>
      assert(c.contractedTerm(pos).sym == contractedSym)
      assert(pos < c.conclusion.terms.size)
    }
    assert(rewrites.nonEmpty, s"Transformation requires at least one rewrited rule")
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
    val c = contractString(contract._1, contract._2)
    val ls = lemmas.map(kv => contractString(kv._1, kv._2))

    s"""${contractedSym.sigString}
       |
       |contract
       |$c
       |
       |rewritings
       |${rewrites.mkString("\n")}
       |
       |lemmas
       |${ls.mkString("\n\n")}
       """.stripMargin
  }

  val verificationTimeout = 10

  lazy val soundnessObligations: Seq[Seq[ProofObligation]] = Soundness.transSoundness(this).map(GoalUnpacking.unpackObligation(_))
  lazy val soundnessResults = soundnessObligations.map(_.map(Verification.verify(_, timeout = verificationTimeout)))
  lazy val isSound = soundnessResults.flatten.forall(_.status == Proved)

  lazy val wellformednessObligations: Seq[ProofObligation] = Wellformedness.wellformedTrans(this)
  lazy val wellformednessResults = wellformednessObligations.map(Verification.verify(_, timeout = verificationTimeout))
  lazy val isWellformed = wellformednessResults.forall(_.status == Proved)

  def apply(kids: Term*): App = App(contractedSym, kids.toList)
}


