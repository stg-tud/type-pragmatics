package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas._

trait ModuleTrans {
	var path: Seq[VeritasConstruct] = Seq()
  
  def testPath(p: VeritasConstruct => Boolean) = path.exists(p)
  
  def trace[VC <: VeritasConstruct, Res](vc: VC)(f: VC => Res) = {
     path = vc +: path
     val res = f(vc)
     path = path.tail
     res
  }
  
  def trace2[VC <: VeritasConstruct, Res](vc: VC)(f: => Res) = {
     path = vc +: path
     val res = f
     path = path.tail
     res
  }
  
  def trace[VC <: VeritasConstruct, Res](vcs: Seq[VC])(f: VC => Seq[Res]) = {
     val res = vcs.flatMap{ vc =>
       path = vc +: path
       val subres = f(vc)
       path = path.tail
       subres
     }
     res
  }
  
  def trans(m: Module): Seq[Module] = trace(m){m => transModule(m.name, m.imports, m.body)} 
  
  def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = 
    Seq(Module(name, trace(is)(transModuleImport(_)), trace(mdefs)(transModuleDef(_))))
  
  def transModuleImport(i: Import) = Seq(i)
  def transModuleBody(mdefs: Seq[ModuleDef]) = mdefs.flatMap(transModuleDef(_))
  def transModuleDef(mdef: ModuleDef) = mdef match {
    case Functions(fs) => trace(fs)(transFunctionDef(_))
    case Axioms(as) => trace(as)(transTypingRule(_))
    case Goals(gs, _) => trace(gs)(transTypingRule(_))
  }
  
  
  def transFunctionDef(fdef: FunctionDef): Seq[ModuleDef] = ???
  def transTypingRule(tr: TypingRule): Seq[ModuleDef] = ???
  
}

trait CollectConstructNames extends ModuleTrans {
  var consNames: Seq[String] = Seq()
  
  def transLocal() = {
    val oldCons = consNames
    ??? // recurse
    consNames = oldCons
  }
  
  def transConstructorDecl(name: String, in: Seq[SortRef], out: SortRef): Seq[ModuleDef] = {
    consNames = consNames :+ name
    Seq(Constructors(Seq(ConstructorDecl(name, in, out))))
  }
  
}

trait FunDesugar extends ModuleTrans with CollectConstructNames {
  
  def withSuper[T, U](xs: Seq[T])(extract: PartialFunction[T, Seq[U]])(f: U => Seq[T]): Seq[T] =
    xs flatMap (t => if (extract.isDefinedAt(t)) extract(t) flatMap f else Seq(t))
  
  override def transModule(name: String, in: Seq[Import], body: Seq[ModuleDef]) = 
    super.transModule(name, in, body) flatMap {
      case Module(name, in, body) => Seq()
    }
    
  
  override def transConstructorDecl(name: String, in: Seq[SortRef], out: SortRef): Seq[ModuleDef] = {
    withSuper(super.transConstructorDecl(name, in, out)) 
        {case Constructors(cs) => cs}
        {case ConstructorDecl(name, in, out) => ???}
    
//    super.transConstructorDecl(name, in, out){case Constructors(cs) => cs}
//    
//    (Constructors, f) 
//    
//    flatMap {
//      case Constructors(cs) => cs flatMap {
//        case ConstructorDecl(name, in, out) => ??? // Seq[ModuleDef]
//      }
//      case mdef => Seq(mdef)
//    }
  }
  
  override def transFunctionDef(fdef: FunctionDef) = 
    if (testPath(_.isInstanceOf[Goals]))
      Seq(Functions(Seq(fdef)))
    else if (consNames.contains("abc"))
      Seq(Functions(Seq(fdef)))
    else 
      ???
  
  
//  def trans(m: Module) = m.fold(
//      n => n, 
//      is => is, 
//      mdefs => mdefs.flatMap(mdef => funDesugarFunctions(mdef)))
//  
//  def funDesugarFunctions(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
//    case Functions(fs) => fs.map (f => funDesugarFunctionDef(f))
//    case _ => Seq(mdef)
//  }
//  
//  def funDesugarFunctionDef(f: FunctionDef): Axioms = f match { 
//    case FunctionDef(sig, eqs) => Axioms(Seq(???))
//  }
  
}