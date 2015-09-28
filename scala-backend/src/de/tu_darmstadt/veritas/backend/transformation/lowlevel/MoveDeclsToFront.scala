package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

object MoveDeclsToFront extends ModuleTransformation {
  var sortdecls: Seq[SortDef] = Seq()
  var consdecls: Seq[ConstructorDecl] = Seq()
  var funcdecls: Seq[FunctionSig] = Seq()
  var pfuncdecls: Seq[FunctionSig] = Seq()

  override def apply(m: Seq[Module]): Seq[Module] = {
    //make sure mutable state is initialized upon application
    sortdecls = Seq()
    consdecls = Seq()
    funcdecls = Seq()
    pfuncdecls = Seq()
    super.apply(m)
  }

  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = {
    val newimps = trace(is)(transModuleImport(_))
    val oldmdefs = trace(mdefs)(transModuleDefs(_))
    val newmdefs = Seq(Sorts(sortdecls)) ++ Seq(Constructors(consdecls)) ++
      Seq(Functions(funcdecls map (fs => FunctionDef(fs, Seq())))) ++
      Seq(PartialFunctions(pfuncdecls map (fs => FunctionDef(fs, Seq())))) ++ oldmdefs

    Seq(Module(name, newimps, newmdefs))
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case Sorts(s)             => { sortdecls ++= s; Seq() }
      case Consts(cts)          => { consdecls ++= cts; Seq() }
      case Constructors(cts)    => { consdecls ++= cts; Seq() }
      case Functions(fs)        => { funcdecls ++= (fs map (fd => fd.signature)); Seq() }
      case PartialFunctions(fs) => { pfuncdecls ++= (fs map (fd => fd.signature)); Seq() }
    }

}