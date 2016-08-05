package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration

/**
 * abstract transformation of a module
 * (using top-down traversal)
 *
 * //TODO: if needed, implement other traversal strategies,
 * inheriting from ModuleTransformation and overriding all methods
 *
 */
trait ModuleTransformation {
  var config: Configuration = _
  
  var path: Seq[VeritasConstruct] = Seq()

  def testPath(p: VeritasConstruct => Boolean) = path.exists(p)

  /**
   * convenience function, can be used whenever overriding one of the trans- functions below
   * in a concrete transformation to execute the "parent transformation" first
   *
   * subtransres is the result of applying the super transformation, f the new transformation that
   * shall be applied in addition, leaving unchanged all constructs for which the f is not defined
   */
  def withSuper[T](suptransres: Seq[T])(f: PartialFunction[T, Seq[T]]): Seq[T] =
    suptransres flatMap (t => if (f.isDefinedAt(t)) f(t) else Seq(t))

  /**
   * convenience function, can be used whenever overriding one of the trans- functions below
   * in a concrete transformation to execute the "parent transformation" first
   *
   * subtransres is the result of applying the super transformation, f the new transformation that
   * shall be applied in addition, leaving unchanged all constructs for which the f is not defined
   */
  def withSuper[T](suptransres: T)(f: PartialFunction[T, T]): T =
    if (f.isDefinedAt(suptransres)) f(suptransres) else suptransres

  /**
   * collect the path of a VeritasConstruct within a given AST
   * parameter f is a transformation for a Veritas construct
   *
   * trace information allows for writing conditional transformations
   * that do different things for a construct depending on the context in which
   * it appears
   *
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
  def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    this.config = config
    val result = m flatMap trans
    this.config = null
    result
  }

  /**
   * top-level transformation for a module
   * (all default implementations simply hand back the construct unchanged!)
   * override what you need, call super to enable mixin compositions
   *
   * naming convention: trans + <Name of constructs> (+ s, if the function can give back a sequence)
   */
  def trans(m: Module): Seq[Module] = {
    path = Seq() //important: reset path for each new module!
    checkPrecondition(m); 
    trace(m) { m => transModule(m.name, m.imports, m.defs) }
  }

  def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
    Seq(Module(name, trace(is)(transModuleImport(_)), trace(mdefs)(transModuleDefs(_))))

  def transModuleImport(i: Import): Seq[Import] = Seq(i)

  def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case Local(defs)                  => Seq(Local(trace(defs)(transModuleDefs(_))))
    case h @ HideAll                  => Seq(h)
    case h @ Hide(rn)                 => Seq(h)
    case i @ Include(rn)              => Seq(i)
    case Strategy(n, i, d)            => Seq(Strategy(n, trace(i)(transModuleImport(_)), trace(d)(transModuleDefs(_))))
    case Axioms(as)                   => Seq(Axioms(trace(as)(transTypingRules(_))))
    case Lemmas(ls, t)                => Seq(Lemmas(trace(ls)(transTypingRules(_)), t))
    case Goals(gs, t)                 => Seq(Goals(trace(gs)(transTypingRules(_)), t))
    case LemmasWithStrategy(s, gs, t) => Seq(LemmasWithStrategy(s, trace(gs)(transTypingRules(_)), t))
    case GoalsWithStrategy(s, gs, t)  => Seq(GoalsWithStrategy(s, trace(gs)(transTypingRules(_)), t))
    case Sorts(s)                     => Seq(Sorts(trace(s)(transSortDefs(_))))
    case Functions(fs)                => Seq(Functions((trace(fs)(transFunctionDefs(_)))))
    case PartialFunctions(fs)         => Seq(PartialFunctions((trace(fs)(transFunctionDefs(_)))))
    case Consts(cts, diff)            => Seq(Consts(trace(cts)(transConstDecl(_)), diff))
    case DataType(open, name, cs)     => Seq(DataType(open, name, trace(cs)(transDataTypeConstructor(_, open, name))))
    // if default case is not covered, compiler shows a warning if the match is not exhaustive
    // look for these warnings when extending the Veritas language!
    //case s                            => throw TransformationError("Unsupported construct in transModuleDef: " + s)
  }

  def transFunctionDefs(fdef: FunctionDef): Seq[FunctionDef] = fdef match {
    case FunctionDef(sig, eqns) => Seq(FunctionDef(trace(sig)(transFunctionSig(_)), trace(eqns)(transFunctionEqs(_))))
  }

  def transFunctionSig(sig: FunctionSig): FunctionSig = sig match {
    case FunctionSig(n, in, out) => FunctionSig(n, trace(in)(transSortRefs(_)), trace(out)(transSortRef(_)))
  }

  def transFunctionEqs(eq: FunctionEq): Seq[FunctionEq] = eq match {
    case FunctionEq(fn, pats, right) => Seq(FunctionEq(fn, trace(pats)(transFunctionPatterns(_)), trace(right)(transFunctionExp(_))))
  }

  def transFunctionPatterns(p: FunctionPattern): Seq[FunctionPattern] = p match {
    case FunctionPatApp(fn, args) => Seq(FunctionPatApp(fn, trace(args)(transFunctionPatterns(_))))
    case pv @ FunctionPatVar(vn)  => Seq(pv)
  }

  def transTypingRules(tr: TypingRule): Seq[TypingRule] = tr match {
    case TypingRule(n, prems, conss) => Seq(TypingRule(n, trace(prems)(transTypingRuleJudgments(_)), trace(conss)(transTypingRuleJudgments(_))))
  }

  def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment = trj match {
    case TypingJudgment(f1, f2, f3)   => TypingJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)), trace(f3)(transFunctionExpMeta(_)))
    case TypingJudgmentSimple(f1, f2) => TypingJudgmentSimple(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)))
    case FunctionExpJudgment(f)       => FunctionExpJudgment(trace(f)(transFunctionExp(_)))
    case ExistsJudgment(vl, jl)       => ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
    case ForallJudgment(vl, jl)       => ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
    case ReduceJudgment(f1, f2)       => ReduceJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)))
    case NotJudgment(jdg)             => NotJudgment(trace(jdg)(transTypingRuleJudgment(_)))
    case OrJudgment(orc)              => OrJudgment(orc map (sor => trace(sor)(transTypingRuleJudgments(_))))
    //case s => throw TransformationError("Unsupported construct in transTypingRuleJudgment: " + s)
  }

  def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = trj match {
    case TypingJudgment(f1, f2, f3)   => Seq(TypingJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)), trace(f3)(transFunctionExpMeta(_))))
    case TypingJudgmentSimple(f1, f2) => Seq(TypingJudgmentSimple(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
    case FunctionExpJudgment(f)       => Seq(FunctionExpJudgment(trace(f)(transFunctionExp(_))))
    case ExistsJudgment(vl, jl)       => Seq(ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_))))
    case ForallJudgment(vl, jl)       => Seq(ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_))))
    case ReduceJudgment(f1, f2)       => Seq(ReduceJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
    case NotJudgment(jdg)             => Seq(NotJudgment(trace(jdg)(transTypingRuleJudgment(_))))
    case OrJudgment(orc)              => Seq(OrJudgment(orc map (sor => trace(sor)(transTypingRuleJudgments(_)))))
    //case s => throw TransformationError("Unsupported construct in transTypingRuleJudgment: " + s)
  }

  def transMetaVar(m: MetaVar): MetaVar = m
  def transMetaVars(m: MetaVar): Seq[MetaVar] = Seq(m)

  def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta = f match {
    case FunctionMeta(m) => FunctionMeta(trace(m)(transMetaVar(_)))
    case f: FunctionExp  => transFunctionExp(f) //no tracing here, we did not yet get out any arguments! (i.e. there is no new information to put into the trace)
  }

  def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] = f match {
    case FunctionMeta(m) => Seq(FunctionMeta(trace(m)(transMetaVar(_))))
    case f: FunctionExp  => transFunctionExps(f) //no tracing here, we did not yet get out any arguments! (i.e. there is no new information to put into the trace)
  }

  def transFunctionExps(f: FunctionExp): Seq[FunctionExp] = f match {
    case FunctionExpNot(f)        => Seq(FunctionExpNot(trace(f)(transFunctionExp(_))))
    case FunctionExpEq(f1, f2)    => Seq(FunctionExpEq(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
    case FunctionExpNeq(f1, f2)   => Seq(FunctionExpNeq(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
    case FunctionExpAnd(l, r)     => Seq(FunctionExpAnd(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_))))
    case FunctionExpOr(l, r)      => Seq(FunctionExpOr(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_))))
    case FunctionExpBiImpl(l, r)  => Seq(FunctionExpBiImpl(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_))))
    case FunctionExpIf(c, t, e)   => Seq(FunctionExpIf(trace(c)(transFunctionExp(_)), trace(t)(transFunctionExpMeta(_)), trace(e)(transFunctionExpMeta(_))))
    case FunctionExpLet(n, e, i)  => Seq(FunctionExpLet(n, trace(e)(transFunctionExpMeta(_)), trace(i)(transFunctionExpMeta(_))))
    case FunctionExpApp(fn, args) => Seq(FunctionExpApp(fn, trace(args)(transFunctionExpMetas(_))))
    case v @ FunctionExpVar(n)    => Seq(v)
    case t @ FunctionExpTrue      => Seq(t)
    case f @ FunctionExpFalse     => Seq(f)
  }

  def transFunctionExp(f: FunctionExp): FunctionExp = f match {
    case FunctionExpNot(f)        => FunctionExpNot(trace(f)(transFunctionExp(_)))
    case FunctionExpEq(f1, f2)    => FunctionExpEq(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)))
    case FunctionExpNeq(f1, f2)   => FunctionExpNeq(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)))
    case FunctionExpAnd(l, r)     => FunctionExpAnd(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_)))
    case FunctionExpOr(l, r)      => FunctionExpOr(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_)))
    case FunctionExpBiImpl(l, r)  => FunctionExpBiImpl(trace(l)(transFunctionExp(_)), trace(r)(transFunctionExp(_)))
    case FunctionExpIf(c, t, e)   => FunctionExpIf(trace(c)(transFunctionExp(_)), trace(t)(transFunctionExpMeta(_)), trace(e)(transFunctionExpMeta(_)))
    case FunctionExpLet(n, e, i)  => FunctionExpLet(n, trace(e)(transFunctionExpMeta(_)), trace(i)(transFunctionExpMeta(_)))
    case FunctionExpApp(fn, args) => FunctionExpApp(fn, (trace(args)(transFunctionExpMetas(_))))
    case v @ FunctionExpVar(n)    => v
    case t @ FunctionExpTrue      => t
    case f @ FunctionExpFalse     => f
  }

  def transSortDefs(sd: SortDef): Seq[SortDef] = Seq(sd)
  
  def transConstDecl(d: ConstDecl): Seq[ConstDecl] = d match {
    case ConstDecl(n, out) => Seq(ConstDecl(n, transSortRef(out)))
  }

  def transDataTypeConstructor(d: DataTypeConstructor, open: Boolean, dataType: String): Seq[DataTypeConstructor] = d match {
    case DataTypeConstructor(n, in) => Seq(DataTypeConstructor(n, trace(in)(transSortRefs(_))))
  }

  def transSortRef(sr: SortRef): SortRef = sr
  def transSortRefs(sr: SortRef): Seq[SortRef] = Seq(sr)

  protected def checkPrecondition(input: Module): Unit = {}

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

object Identity extends ModuleTransformation
