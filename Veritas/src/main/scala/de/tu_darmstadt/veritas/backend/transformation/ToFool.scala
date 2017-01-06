package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpIf
import de.tu_darmstadt.veritas.backend.fof.FofUnitary
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpLet
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExp
import de.tu_darmstadt.veritas.backend.fof.Parenthesized
import de.tu_darmstadt.veritas.backend.fof.UntypedFunSymbol
import de.tu_darmstadt.veritas.backend.fof.Appl
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.fof.UntypedVariable
import de.tu_darmstadt.veritas.backend.fof.Term
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.fool.IfThenElseFofUnitary
import de.tu_darmstadt.veritas.backend.fool.LetInFofUnitary
import de.tu_darmstadt.veritas.backend.fool.LetInTerm
import de.tu_darmstadt.veritas.backend.fool.IfThenElseTerm
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpVar

class ToFool extends ToTff {

  override def functionExpToTff(f: FunctionExp): FofUnitary = {
    try {
      f match {
        case FunctionExpIf(g, t, e)  => Parenthesized(IfThenElseFofUnitary(functionExpToTff(g), functionExpToTff(t.asInstanceOf[FunctionExp]), functionExpToTff(e.asInstanceOf[FunctionExp])))
        case FunctionExpLet(v, e, b) => Parenthesized(LetInFofUnitary(Seq((UntypedFunSymbol(v), functionExpMetaToTff(e))), functionExpToTff(b.asInstanceOf[FunctionExp])))
        case _                       => super.functionExpToTff(f)
      }
    } catch {
      case c: ClassCastException => throw TransformationError("In the following function expression, a construct either contained illegal meta variables, or unresolved constructor variables: " + f)
      case e: Exception          => throw e
    }
  }

  override def functionExpMetaToTff(f: FunctionExpMeta): Term =
    // the only constructs which can be turned into a term are
    // FunctionMeta and FunctionExpApp (Appl is both a Term and a FofUnitary!), FunctionExpIf and FunctionExpLet
    // therefore, encountering any other FunctionExpMeta must result in an error!
    f match {
      case FunctionMeta(MetaVar(m)) => UntypedVariable(m)
      case FunctionExpVar(n)        => UntypedFunSymbol(n) //can occur inside lets, but should not occur elsewhere!
      case FunctionExpApp(n, args)  => Appl(UntypedFunSymbol(n), args map functionExpMetaToTff)
      case FunctionExpIf(g, t, e)   => IfThenElseTerm(functionExpToTff(g), functionExpMetaToTff(t), functionExpMetaToTff(e))
      case FunctionExpLet(v, e, b)  => LetInTerm(Seq((UntypedFunSymbol(v), functionExpMetaToTff(e))), functionExpMetaToTff(b))
      case _                        => throw TransformationError(s"Encountered unexpected construct in functionExpMetaToTff: $f")
    }

}

object ToFool {
  def apply(m: Module)(implicit config: Configuration) = (new ToFool).toTffFile(m)(config)
}