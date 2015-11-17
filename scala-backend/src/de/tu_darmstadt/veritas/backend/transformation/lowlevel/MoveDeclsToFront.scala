package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError

/**
 * moves declarations of sorts, constructors, functions etc. to the front of the module 
 * 
 * assumes that the module does not contain anymore local or strategy blocks!
 */
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
    val newmdefs = Seq(Sorts(sortdecls.distinct)) ++ Seq(Constructors(consdecls.distinct)) ++
      Seq(Functions(funcdecls.distinct map (fs => FunctionDef(fs, Seq())))) ++
      Seq(PartialFunctions(pfuncdecls.distinct map (fs => FunctionDef(fs, Seq())))) ++ oldmdefs

    Seq(Module(name, newimps, newmdefs))
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case Sorts(s)             => { sortdecls ++= s; Seq() }
      case Consts(cts)          => { consdecls ++= cts; Seq() }
      case Constructors(cts)    => { consdecls ++= cts; Seq() }
      case Functions(fs)        => { funcdecls ++= (fs map (fd => fd.signature)); Seq() }
      case PartialFunctions(fs) => { pfuncdecls ++= (fs map (fd => fd.signature)); Seq() }
      case Local(_) => throw TransformationError("Local-block not expected in transformation MoveDeclsToFront!")
      case Strategy(_, _, _) => throw TransformationError("Strategy-block not expected in transformation MoveDeclsToFront!")
    }

}