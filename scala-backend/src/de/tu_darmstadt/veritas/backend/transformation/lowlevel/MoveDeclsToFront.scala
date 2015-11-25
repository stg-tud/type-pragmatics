package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.Configuration

/**
 * moves declarations of sorts, constructors, functions etc. to the front of the module 
 * 
 * assumes that the module does not contain anymore local or strategy blocks!
 */
object MoveDeclsToFront extends ModuleTransformation {
  var sortdecls: Seq[Sorts] = Seq()
  var constdecls: Seq[Consts] = Seq()
  var datatypes: Seq[DataType] = Seq()
  var funcdecls: Seq[Functions] = Seq()
  var pfuncdecls: Seq[PartialFunctions] = Seq()

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure mutable state is initialized upon application
    sortdecls = Seq()
    constdecls = Seq()
    funcdecls = Seq()
    pfuncdecls = Seq()
    super.apply(m)
  }

  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = {
    val newimps = trace(is)(transModuleImport(_))
    val oldmdefs = trace(mdefs)(transModuleDefs(_))
    val newmdefs = (sortdecls ++ datatypes ++ constdecls ++ funcdecls ++ pfuncdecls).distinct ++ oldmdefs

    Seq(Module(name, newimps, newmdefs))
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case s@Sorts(_)             => { sortdecls :+= s; Seq() }
      case cs@Consts(_,_)          => { constdecls :+= cs; Seq() }
      case dt@DataType(_,_,_)   => { 
        datatypes :+= dt; Seq() 
      }
      case Functions(fs)        => { funcdecls :+= Functions(fs map (f => FunctionDef(f.signature, Seq()))); Seq() }
      case PartialFunctions(fs) => { pfuncdecls :+= PartialFunctions(fs map (f => FunctionDef(f.signature, Seq()))); Seq() }
      case Local(_) => throw TransformationError("Local-block not expected in transformation MoveDeclsToFront!")
      case Strategy(_, _, _) => throw TransformationError("Strategy-block not expected in transformation MoveDeclsToFront!")
    }

}