package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

/**
 * generates axioms for function equations
 * takes order of function equations into account!
 * --> assumes that FunctionPatVar/FunctionExpVar was already substituted with
 * FunctionPatApp/FunctionExpApp if there was a clash with constructor names!
 * TODO Is it possible to generate a simple precondition with this requirement? Probably not...
 */
object FunctionEqToAxioms extends ModuleTransformation {

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case Functions(fs) =>
        //generate one block with all the function signatures
        //then a list of axioms from all the function equations
        Seq(Functions(fs map { case FunctionDef(sig, _) => FunctionDef(sig, Seq()) })) ++
          fs flatMap { case FunctionDef(sig, feqs) => functionEqsDatas(feqs, makeArgMetaVars(sig.in), sig.out, Seq()) }
    }

  private def makeArgMetaVars(args: Seq[SortRef]): Seq[MetaVar] = ???

  private def functionPatProp(fp: FunctionPattern): FunctionExp = ???
  
  private def notMatchPrepatsProp(pats: Seq[FunctionPattern], prepats: Seq[FunctionPattern]): FunctionExp = ???
  
  private def functionEqData(fe: FunctionEq, argvars: Seq[MetaVar], restype: SortRef, preprop: FunctionExp): Seq[TypingRule] = ???

  private def functionEqsDatas(feqs: Seq[FunctionEq], argvars: Seq[MetaVar], restype: SortRef, prepats: Seq[FunctionPattern]): Seq[Axioms] = {
    feqs match {
      case Nil => Nil
      case (f @ FunctionEq(name, pats, exp)) :: fs => {
        val patprops = pats map functionPatProp
        val patbindprops = (argvars.zip(patprops)) map { case (a, p) => FunctionExpEq(FunctionMeta(a), p) }
        val patprop: FunctionExp = patbindprops.fold(FunctionExpTrue)((e1, e2) => FunctionExpAnd(e1, e2))
        val notmatchprepatsprop = notMatchPrepatsProp(pats, prepats)
        val patnotprepatprop = FunctionExpAnd(patprop, notmatchprepatsprop)
        val functiondata = functionEqData(f, argvars, restype, patnotprepatprop)
        val restdata = functionEqsDatas(fs, argvars, restype, pats ++ prepats)
        Seq(Axioms(functiondata)) ++ restdata
      }
    }
  }
}