package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpIf
import de.tu_darmstadt.veritas.backend.fof.FofUnitary
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpLet
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExp
import de.tu_darmstadt.veritas.backend.fof.Parenthesized
import de.tu_darmstadt.veritas.backend.fool.IfThenElse
import de.tu_darmstadt.veritas.backend.fool.LetIn
import de.tu_darmstadt.veritas.backend.fof.UntypedFunSymbol

class ToFool extends ToTff {

  override def functionExpToTff(f: FunctionExp): FofUnitary = {
    f match {
      //TODO think about more safe conversion of if-guards!!
      //if guards should not contain lone variables?
      case FunctionExpIf(g, t, e)  => Parenthesized(IfThenElse(functionExpMetaToTff(g).asInstanceOf[FofUnitary], functionExpMetaToTff(t), functionExpMetaToTff(e)))
      case FunctionExpLet(v, e, b) => Parenthesized(LetIn(Seq((UntypedFunSymbol(v), functionExpMetaToTff(e))), functionExpMetaToTff(b)))
      case _                       => super.functionExpToTff(f)
    }
  }

}

object ToFool {
  def apply(m: Module)(implicit config: Configuration) = (new ToFool).toTffFile(m)(config)
}