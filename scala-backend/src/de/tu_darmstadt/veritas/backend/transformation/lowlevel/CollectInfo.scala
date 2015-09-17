package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

trait CollectConstructorNames extends ModuleTransformation {
  var consNames: Seq[String] = Seq()

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    withSuper(super.transModuleDefs(mdef)) {
      case Local(defs) => {
        val oldCons = consNames
        val res = Seq(Local(trace(defs)(transModuleDefs(_))))
        consNames = oldCons
        res
      }
      case Strategy(n, i, d) => {
        val oldCons = consNames
        val res = Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDefs(_))))
        consNames = oldCons
        res
      }
    }

  override def transConstructorDecls(cd: ConstructorDecl): Seq[ConstructorDecl] =
    withSuper(super.transConstructorDecls(cd)) {
      case c @ ConstructorDecl(n, in, out) => {
        consNames = consNames :+ cd.name
        Seq(c)
      }
    }

}