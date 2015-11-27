package de.tu_darmstadt.veritas.backend.transformation.collect

import TypeInference._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._

object TypeInference {
  trait Type
  case class UVar(name: String) extends Type
  case class Sort(name: String) extends Type
  case class Function(in: Seq[Type], out: Type) extends Type
  
  val Bool = Sort("Bool")
  
  trait Constraint
  case class Eq(t1: Type, t2: Type) extends Constraint
  
  case class TypeError(msg: String) extends BackendError(msg)
}


class CollectTypeConstraints extends CollectTypes {
  private val fresh = new FreshNames
  
  private var cons = Seq[Constraint]()
  def constraints = cons
  
  private var context = Map[MetaVar, Type]()
  private var vcontext = Map[String, Type]()
  
  def typeIs(v: MetaVar, t: Type) {
    context.get(v) match {
      case Some(t2) => cons :+= Eq(t, t2)
      case None => context += (v -> t)
    }
  }
  
  def typeIs(v: String, t: Type) {
    vcontext.get(v) match {
      case Some(t2) => cons :+= Eq(t, t2)
      case None => vcontext += (v -> t)
    }
  }
  
  def typeIs(t1: Type, t2: Type) {
    cons :+= Eq(t1, t2)
  }
  
  def withType[T](v: String, t: Type)(f: => T): T = {
    val oldvcontext = vcontext
    vcontext += (v -> t)
    val res = f
    vcontext = oldvcontext
    res
  }
  
  def freshType(v: String): Type = {
    val t = UVar(fresh.freshName("T"))
    vcontext += (v -> t)
    t
  }
  
  override def transModuleImport(i: Import): Seq[Import] = 
    throw BackendError(s"Type inference cannot handle imports. Seen $i")
  
  /**
   * Make sure that type symbols are properly scoped by local and strategy blocks
   */
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case Local(_) | Strategy(_,_,_) => 
      val oldcontext = context
      val res = super.transModuleDefs(mdef)
      context = oldcontext
      res
    case _ => 
      super.transModuleDefs(mdef)
  }
  
  /**
   * This is the entry point for all expressions occurring in judgments.
   * We cancel the visitor at this point and use our own traversal.
   */
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta = f match {
    case FunctionMeta(m) => 
      typeIs(m, Bool)
      f
    case f: FunctionExp  => 
      typeIs(check(f), Bool)
      f
  }
  
  def check(e: FunctionExpMeta): Type = e match {
    case FunctionMeta(v) =>
      context.get(v) match {
        case Some(t) => t
        case None => throw TypeError(s"Cannot find type of metavariable symbol $v")
      }
    case FunctionExpVar(n) =>
      vcontext.get(n) match {
        case Some(t) => t
        case None => throw TypeError(s"Cannot find type of variable symbol $n")
      }
    case FunctionExpNot(e) => 
      typeIs(check(e), Bool)
      Bool
    case FunctionExpEq(e1, e2)    => 
      typeIs(check(e1), check(e2))
      Bool
    case FunctionExpNeq(e1, e2)   => 
      typeIs(check(e1), check(e2))
      Bool
    case FunctionExpAnd(e1, e2) => 
      typeIs(check(e1), Bool)
      typeIs(check(e2), Bool)
      Bool
    case FunctionExpOr(e1, e2) =>
      typeIs(check(e1), Bool)
      typeIs(check(e2), Bool)
      Bool
    case FunctionExpBiImpl(e1, e2)  => 
      typeIs(check(e1), Bool)
      typeIs(check(e2), Bool)
      Bool
    case FunctionExpIf(c, t, e)   => 
      typeIs(check(c), Bool)
      val ty = check(t)
      typeIs(ty, check(e))
      ty
    case FunctionExpLet(n, e, i)  => 
      val te = check(e)
      withType(n, te) {
        check(i)
      }
    case FunctionExpApp(fn, args) => 
      symbolType(fn) match {
        case Some((ins, out)) => 
          args.zip(ins) foreach { ta =>
            typeIs(check(ta._1), Sort(ta._2.name))
          }
          Sort(out.name)
        case None =>
          throw TypeError(s"Cannot find type of function symbol $fn")
      }
    case FunctionExpTrue      => 
      Bool
    case FunctionExpFalse     => 
      Bool
  }

  
}