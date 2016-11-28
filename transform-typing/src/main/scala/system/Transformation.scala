package system

import system.Syntax._
import system.Verification.Obligation

case class Transformation(lang: Language, contract: Rule, pos: Int, rewrites: List[Rewrite]) {
  assert(pos < contract.conclusion.terms.size)
  assert(contract.conclusion.terms(pos).isInstanceOf[App])

  val contractedTerm = contract.conclusion.terms(pos).asInstanceOf[App]
  val contractedSym = contractedTerm.sym

  rewrites.foreach(r =>
    assert(
      r.pat.isInstanceOf[App] && r.pat.asInstanceOf[App].sym == contractedSym,
      s"Rewrite $r does not match contracted symbol $contractedSym"))

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
       |${rewrites.mkString("\n")}
       """.stripMargin
  }


  lazy val soundnessObligations: Seq[Obligation] = Soundness.transSoundness(this)
  lazy val soundnessResults = ???
  def isSound: Boolean = ???
}

object Transformation {
  def apply(lang: Language, contract: Rule, pos: Int, rewrites: Rewrite*): Transformation =
    new Transformation(lang, contract, pos, rewrites.toList)
}
