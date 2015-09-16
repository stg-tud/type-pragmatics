package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas._

/**
 * abstract transformation of a module
 * (using top-down traversal)
 *
 * //TODO: if needed, implement other traversal strategies,
 * inheriting from ModuleTransformation and overriding all methods
 *
 */
trait ModuleTransformation {
  var path: Seq[VeritasConstruct] = Seq()

  def testPath(p: VeritasConstruct => Boolean) = path.exists(p)

  /**
   * collect the path of a VeritasConstruct within a given AST
   * parameter f is a transformation for a Veritas construct
   */
  def trace[VC <: VeritasConstruct, Res](vc: VC)(f: VC => Res) = {
    path = vc +: path
    val res = f(vc)
    path = path.tail
    res
  }

  /**
   * collect the path of a sequence of VeritasConstruct within a given AST
   * parameter f is a transformation for a Veritas construct
   */
  def trace[VC <: VeritasConstruct, Res](vcs: Seq[VC])(f: VC => Seq[Res]) = {
    val res = vcs.flatMap { vc =>
      path = vc +: path
      val subres = f(vc)
      path = path.tail
      subres
    }
    res
  }

  /**
   * variant of trace function, but gives back a result directly
   * -> not yet sure whether this is useful
   */
  def trace2[VC <: VeritasConstruct, Res](vc: VC)(f: => Res) = {
    path = vc +: path
    val res = f
    path = path.tail
    res
  }

  /**
   * convenience method for applying the transformation to a sequence of modules in a row
   */
  def apply(m: Seq[Module]): Seq[Module] = m flatMap trans

  /**
   * top-level transformation for a module
   * (all default implementations simply hand back the construct unchanged!)
   * override what you need, call super to enable mixin compositions
   */
  def trans(m: Module): Seq[Module] = trace(m) { m => transModule(m.name, m.imports, m.body) }

  def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
    Seq(Module(name, trace(is)(transModuleImport(_)), trace(mdefs)(transModuleDef(_))))

  def transModuleImport(i: Import): Seq[Import] = Seq(i)
  def transModuleBody(mdefs: Seq[ModuleDef]): Seq[ModuleDef] = mdefs.flatMap(transModuleDef(_))
  def transModuleDef(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case Local(defs)     => Seq(Local(trace(defs)(transModuleDef(_))))
    case h @ HideAll     => Seq(h)
    case h @ Hide(rn)    => Seq(h)
    case i @ Include(rn) => Seq(i)
    case Strategy(n, i, d) => Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDef(_))))
    case Functions(fs)   => Seq(Functions((trace(fs)(transFunctionDef(_)))))
    case Axioms(as)      => Seq(Axioms(trace(as)(transTypingRule(_))))
    case Lemmas(ls, t)    => Seq(Lemmas(trace(ls)(transTypingRule(_)), t))
    case Goals(gs, t)    => Seq(Goals(trace(gs)(transTypingRule(_)), t))
    case LemmasWithStrategy(s, gs, t) => Seq(LemmasWithStrategy(s, trace(gs)(transTypingRule(_)), t))
    case GoalsWithStrategy(s, gs, t) => Seq(GoalsWithStrategy(s, trace(gs)(transTypingRule(_)), t))
    case Sorts(s) => ???
  }

  def transFunctionDef(fdef: FunctionDef): Seq[FunctionDef] = ???
  def transTypingRule(tr: TypingRule): Seq[TypingRule] = ???
}

//trait ModuleDefTransformation extends ModuleTransformation {
//  final override def apply(input: Module): Seq[Module] = {
//    checkPrecondition(input)
//    Seq(Module(input.name,
//           input.imports,
//           input.body.flatMap(apply orElse { case m => Seq(m) })))
//  }
//  
//  /**
//   * override this method when implementing new module transformations
//   * method can do partial pattern matching on the constructs of interest
//   */
//  protected /* abstract */ def apply: PartialFunction[ModuleDef, Seq[ModuleDef]]
//  
//  /**
//   * Override this if you want to validate the input Module
//   */
//  protected def checkPrecondition(input: Module): Unit = {}
//}