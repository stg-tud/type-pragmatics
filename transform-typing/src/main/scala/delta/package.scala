import stlc.Syntax.Num
import system.LanguageExtension
import system.Syntax.{Name, Symbol}

package object delta {
  val zero = Symbol("zero", in = List(), out = Num, constr = false)
  val d = Symbol("d", in = List(Name), out = Name, constr = true)
  val v = Symbol("v", in = List(Name), out = Name, constr = true)

  object ext extends LanguageExtension("delta-ext", Seq(), Seq(zero, d, v), Seq())
}
