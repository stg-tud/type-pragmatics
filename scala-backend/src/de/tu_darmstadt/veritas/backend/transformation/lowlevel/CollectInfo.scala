package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import scala.collection.mutable.Map
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

trait CollectSortNames extends ModuleTransformation {
  var sortNames: Seq[String] = Seq()

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    withSuper(super.transModuleDefs(mdef)) {
      case Local(defs) => {
        val oldCons = sortNames
        val res = Seq(Local(trace(defs)(transModuleDefs(_))))
        sortNames = oldCons
        res
      }
      case Strategy(n, i, d) => {
        val oldCons = sortNames
        val res = Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDefs(_))))
        sortNames = oldCons
        res
      }
    }

  override def transSortDefs(sd: SortDef): Seq[SortDef] =
    withSuper(super.transSortDefs(sd)) {
      case s @ SortDef(_) => {
        sortNames = sortNames :+ sd.name
        Seq(s)
      }
    }

}

trait CollectTypeInfo extends ModuleTransformation {
  var constypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  var functypes: Map[String, (Seq[SortRef], SortRef)] = Map()

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    withSuper(super.transModuleDefs(mdef)) {
      case cons @ Constructors(cdecl) => {
        (cdecl map { case c @ ConstructorDecl(n, in, out) => constypes.put(n, (in, out)) })
        Seq(cons)
      }
      case funcs @ Functions(fdecl) => {
        (fdecl map { case c @ FunctionDef(FunctionSig(n, in, out), defs) => functypes.put(n, (in, out)) })
        Seq(funcs)
      }
      case Local(defs) => {
        val oldconstypes = constypes
        val oldfunctypes = functypes
        val res = Seq(Local(trace(defs)(transModuleDefs(_))))
        constypes = oldconstypes
        functypes = oldfunctypes
        res
      }
      case Strategy(n, i, d) => {
        val oldconstypes = constypes
        val oldfunctypes = functypes
        val res = Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDefs(_))))
        constypes = oldconstypes
        functypes = oldfunctypes
        res
      }
    }
}

