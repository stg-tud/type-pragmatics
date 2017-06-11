package system

import system.Syntax._
import system.Verification.ProofObligation
import veritas.benchmarking.Proved

import scala.collection.immutable.ListMap

abstract class Transformation(val lang: Language) {
  val contract: (Rule, Int)
  //def contractPos: Option[Int] = contract._1.conclusion.terms.filter()
  val lemmas: ListMap[Rule, Int] = ListMap()
  val rewrites: Seq[Rewrite]

  lazy val rules: ListMap[Rule, Int] = ListMap(contract) ++ lemmas

//  final lazy val contractedTerms = contracts.map{case (c,pos) => c.conclusion.terms(pos).asInstanceOf[App]}
  lazy val contractedSym = rewrites.head.sym
  final lazy val contractedTerm = contract._1.contractedTerm(contract._2)


  lazy val undeclaredSymbols = {
    val lsyms = rules.foldLeft(Set[Symbol]())((set, c) => set ++ c._1.symbols)
    val rsyms = rewrites.foldLeft(Set[Symbol]())((set, r) => set ++ r.symbols)
    val otherTransSyms = lang.transs.map(t => t.contractedSym).toSet
    (lsyms++rsyms).diff(lang.syms.toSet).diff(lang.undeclaredSymbols).diff(otherTransSyms) - contractedSym
  }

  def checkSyntax(): Unit = {
    rules.foreach(kv => assert(!contractedSym.constr, s"Transformation symbol must not be marked as constructor $contractedSym"))
    rules.foreach(kv => assert(kv._1.isLemma, s"Transformation contracts must be marked as lemmas"))
    rules.foreach { case (c, pos) =>
      assert(c.conclusion.terms(pos).isInstanceOf[App], s"contracted term should be an application but was ${c.conclusion.terms(pos)} in ${c.name}")
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

  def forall[T](f: Transformation => T): ListMap[Symbol, T] =
    ListMap() ++ lang.transs.map(t => t.contractedSym -> f(t)) + (contractedSym -> f(this))

  lazy val failedProofs = (contractComplianceFailed ++ soundnessFailed ++ completenessFailed).toList
  lazy val isOk = failedProofs.isEmpty
  def allOk = forall(_.isOk).forall(_._2)

  val soundnessTimeout = 30
  val soundnessMode = "casc"
  lazy val soundnessObligations: Seq[Seq[ProofObligation]] = Soundness.soundnessTrans(this).toStream.map(_.optimized)
  lazy val soundnessResults = soundnessObligations.map(_.map(Verification.verify(_, soundnessMode, soundnessTimeout)))
  lazy val isSound = soundnessResults.flatten.forall(_.status == Proved)
  def soundnessFailed = soundnessResults.flatten.zip(soundnessObligations.flatten).filter(_._1.status != Proved).map{case (res, obl) => res.file -> obl}

  val contractComplianceTimeout = 30
  val contractComplianceMode = "casc"
  lazy val contractComplianceObligations: Seq[Seq[ProofObligation]] = ContractCompliance.complianceTrans(this).toStream.map(_.optimized)
  lazy val contractComplianceResults = contractComplianceObligations.map(_.map(Verification.verify(_, contractComplianceMode, contractComplianceTimeout)))
  lazy val isContractCompliant = contractComplianceResults.flatten.forall(_.status == Proved)
  def contractComplianceFailed = contractComplianceResults.flatten.zip(contractComplianceObligations.flatten).filter(_._1.status != Proved).map{case (res, obl) => res.file -> obl}

  val completenessTimeout = 30
  val completenessMode = "casc"
  lazy val completenessObligations: Seq[Seq[ProofObligation]] = Completeness.completenessTrans(this).toStream.map(_.optimized)
  lazy val completenessResults = completenessObligations.map(_.map(Verification.verify(_, completenessMode, completenessTimeout)))
  lazy val isComplete = completenessResults.flatten.forall(_.status == Proved)
  def completenessFailed = completenessResults.flatten.zip(completenessObligations.flatten).filter(_._1.status != Proved).map{case (res, obl) => res.file -> obl}

  def apply(kids: Term*): App = App(contractedSym, kids.toList)
}


