package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.Configuration

trait CollectConstructorNames extends ModuleTransformation {
  var consNames: Set[String] = Set()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    consNames = Set()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case l @ Local(defs) => {
        val oldCons = consNames
        val res = super.transModuleDefs(l)
        consNames = oldCons
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldCons = consNames
        val res = super.transModuleDefs(s)
        consNames = oldCons
        res
      }
      case m => super.transModuleDefs(m)
    }

  override def transConstructorDecls(cd: ConstructorDecl, const: Boolean): Seq[ConstructorDecl] =
    withSuper(super.transConstructorDecls(cd, const)) {
      case c @ ConstructorDecl(n, in, out) => {
        consNames = consNames + cd.name
        Seq(c)
      }
    }

}

trait CollectSortNames extends ModuleTransformation {
  var sortNames: Set[String] = Set()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    sortNames = Set()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case l @ Local(defs) => {
        val oldCons = sortNames
        val res = super.transModuleDefs(l)
        sortNames = oldCons
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldCons = sortNames
        val res = super.transModuleDefs(s)
        sortNames = oldCons
        res
      }
      case m => super.transModuleDefs(m)
    }

  override def transSortDefs(sd: SortDef): Seq[SortDef] =
    withSuper(super.transSortDefs(sd)) {
      case s @ SortDef(_) => {
        sortNames = sortNames + sd.name
        Seq(s)
      }
    }

}

trait CollectTypeInfo extends ModuleTransformation {
  var constypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  var functypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  var pfunctypes: Map[String, (Seq[SortRef], SortRef)] = Map()
  
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    constypes = Map()
    functypes = Map()
    pfunctypes = Map()
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    //take scoping into account (local blocks and strategy blocks!)
    //reset consNames to previous value when "leaving" such a block during the traversal
    mdef match {
      case cons @ Constructors(cdecl) => {
        (cdecl map { case c @ ConstructorDecl(n, in, out) => constypes.put(n, (in, out)) })
        Seq(Constructors(trace(cdecl)(transConstructorDecls(_, false))))
      }
      case funcs @ Functions(fdecl) => {
        (fdecl map { case c @ FunctionDef(FunctionSig(n, in, out), defs) => functypes.put(n, (in, out)) })
        Seq(Functions((trace(fdecl)(transFunctionDefs(_)))))
      }
      case pf @ PartialFunctions(fdecl) => {
        (fdecl map { case c @ FunctionDef(FunctionSig(n, in, out), defs) => pfunctypes.put(n, (in, out)) })
        Seq(PartialFunctions((trace(fdecl)(transFunctionDefs(_)))))
      }
      case l @ Local(defs) => {
        val oldconstypes = constypes
        val oldfunctypes = functypes
        val oldpfunctypes = pfunctypes
        val res = Seq(Local(trace(defs)(transModuleDefs(_))))
        constypes = oldconstypes
        functypes = oldfunctypes
        pfunctypes = oldpfunctypes
        res
      }
      case s @ Strategy(n, i, d) => {
        val oldconstypes = constypes
        val oldfunctypes = functypes
        val oldpfunctypes = pfunctypes
        val res = Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDefs(_))))
        constypes = oldconstypes
        functypes = oldfunctypes
        pfunctypes = oldpfunctypes
        res
      }
      case m => super.transModuleDefs(m)
    }
}

