package system

import system.Syntax._
import system.Verification.Obligation
import veritas.benchmarking.Proved

abstract class Transformation(val lang: Language) {
  val contract: Rule
  val contractPos: Int
  val rewrites: Seq[Rewrite]

  final lazy val contractedTerm = contract.conclusion.terms(contractPos).asInstanceOf[App]
  final lazy val contractedSym = contractedTerm.sym

  def checkSyntax(): Unit = {
    assert(contractPos < contract.conclusion.terms.size)
    assert(contract.conclusion.terms(contractPos).isInstanceOf[App])
    rewrites.foreach(r =>
      assert(
        r.pat.isInstanceOf[App] && r.pat.asInstanceOf[App].sym == contractedSym,
        s"Rewrite $r does not match contracted symbol $contractedSym"))
  }

  override def toString: String = {
    val premises = contract.premises
    val name = contract.name
    val indent = "  "
    val ps = premises.mkString("\n" + indent)
    val scontract = s"$name:\n$indent$ps\n$indent=>\n$indent${contract.conclusion.toString(contractPos)}"
    s"""${contractedSym.sigString}
       |
         |contract
       |$scontract
       |
         |rewritings
       |${rewrites.mkString("\n")}
       """.stripMargin
  }

  lazy val soundnessObligations: Seq[Obligation] = Soundness.transSoundness(this)
  lazy val soundnessResults = soundnessObligations.map(Verification.verify(_))
  lazy val isSound = soundnessResults.forall(_.status == Proved)
}


