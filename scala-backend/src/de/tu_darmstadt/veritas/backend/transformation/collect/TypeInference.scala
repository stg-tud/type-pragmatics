package de.tu_darmstadt.veritas.backend.transformation.collect

import TypeInference._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._

object TypeInference {
  type USubst = Map[UVar, Type]
  
  trait Type {
    def unify(t: Type, s: USubst): Option[USubst]
    def subst(s: USubst): Type
  }
  case class UVar(name: String) extends Type {
    def unify(t: Type, s: USubst) =
      s.get(this) match {
        case Some(t1) => t1.unify(t, s)
        case None => Some(Map(this -> t.subst(s)))
      }
    def subst(s: USubst) = s.getOrElse(this, this)
  }
  case class Sort(name: String) extends Type {
    def unify(t: Type, s: USubst) = t match {
      case Sort(`name`) => Some(Map())
      case _ => None
    }
    def subst(s: USubst): Type = this
  }
  case class Function(in: Seq[Type], out: Type) extends Type {
    def unify(t: Type, s: USubst): Option[USubst] = t match {
      case Function(in2, out2) => 
        var currents = s
        var res: USubst = Map()
        val ls = in :+ out
        val rs = in2 :+ out2
        ls.zip(rs) foreach { tt =>
          tt._1.unify(tt._2, currents) match {
            case None => return None
            case Some(news) =>
              currents ++= news
              res ++= news
          }
        }
        Some(res)
      case _ => None
    }
    def subst(s: USubst): Type = Function(in map (_.subst(s)), out.subst(s))
  }
  
  val Bool = Sort("Bool")
  
  trait Constraint
  case class Eq(t1: Type, t2: Type) extends Constraint
  
  case class TypeError(msg: String) extends BackendError("Type inference failed. " + msg)
}


class TypeInference(symbolType: String => Option[(Seq[SortRef],SortRef)]) {
  private val fresh = new FreshNames
  
  private var cons = Seq[Constraint]()
  def constraints = cons
  
  private var metacontext = Map[MetaVar, Type]()
  private var varcontext = Map[String, Type]()
  
  def typeIs(v: MetaVar, t: Type) {
    metacontext.get(v) match {
      case Some(t2) => cons :+= Eq(t, t2)
      case None => metacontext += (v -> t)
    }
  }
  
  def typeIs(t1: Type, t2: Type) {
    cons :+= Eq(t1, t2)
  }
  
  def withType[T](v: String, t: Type)(f: => T): T = {
    val oldvarcontext = varcontext
    varcontext += (v -> t)
    val res = f
    varcontext = oldvarcontext
    res
  }
  
  def freshType(v: MetaVar): Type = {
    val t = UVar(fresh.freshName("T"))
    metacontext += (v -> t)
    t
  }
  
  def inferMetavarTypes(vars: Iterable[MetaVar], jdgs: Seq[TypingRuleJudgment]): Map[MetaVar, SortRef] = {
    jdgs foreach (checkJudgment(_))
    
    vars foreach { v => 
      if (!metacontext.contains(v))
        throw TypeError(s"Could not infer type of metavariable ${v.name}, no candidate found.")
    }
    
    val solveRes = solveConstraints()
    if (solveRes._2.nonEmpty)
      throw TypeError(s"Could not solve the following constraints: ${solveRes._2}")
    
    var res = Map[MetaVar, SortRef]()
    vars foreach { v =>
      metacontext(v).subst(solveRes._1) match {
        case Sort(t) => res += v -> SortRef(t)
        case t => throw TypeError(s"Found complex type for metavariable $v: $t")
      }
    }
    res
  }
  
  def checkJudgment(jdg: TypingRuleJudgment): Unit = jdg match {
    case FunctionExpJudgment(f) => checkExpBool(f)
    case ExistsJudgment(vl, jdgl) => checkJudgmentScope(vl, jdgl)
    case ForallJudgment(vl, jdgl) => checkJudgmentScope(vl, jdgl)
    case NotJudgment(jdg) => checkJudgment(jdg)
    case OrJudgment(jdgll) => jdgll foreach (jdgl => jdgl foreach (checkJudgment(_)))
    case ReduceJudgment(e1, e2) => checkExp(e1); checkExp(e2)
    case TypingJudgment(e1, e2 ,e3) => checkExp(e1); checkExp(e2); checkExp(e3)
    case TypingJudgmentSimple(e1, e2) => checkExp(e1); checkExp(e2)
  }
  
  def checkJudgmentScope(vl: Seq[MetaVar], jdgl: Seq[TypingRuleJudgment]) {
    val oldvlbindings = vl map (v => v -> metacontext.get(v))
    metacontext --= vl
    jdgl foreach (checkJudgment(_))
    metacontext --= vl
    oldvlbindings foreach {
      case (v, Some(t)) => metacontext += v -> t
      case _ => {}
    }
  }
  
  /**
   * This is the entry point for all expressions occurring as FunctionExpJudgment.
   */
  def checkExpBool(f: FunctionExpMeta): Type = f match {
    case FunctionMeta(m) => 
      typeIs(m, Bool)
      Bool
    case f: FunctionExp  => 
      typeIs(checkExp(f), Bool)
      Bool
  }
  
  def checkExp(e: FunctionExpMeta): Type = e match {
    case FunctionMeta(v) =>
      metacontext.get(v) match {
        case Some(t) => t
        case None => freshType(v)
      }
    case FunctionExpVar(n) =>
      varcontext.get(n) match {
        case Some(t) => t
        case None => throw TypeError(s"Cannot find type of variable symbol $n")
      }
    case FunctionExpNot(e) => 
      typeIs(checkExp(e), Bool)
      Bool
    case FunctionExpEq(e1, e2)    => 
      typeIs(checkExp(e1), checkExp(e2))
      Bool
    case FunctionExpNeq(e1, e2)   => 
      typeIs(checkExp(e1), checkExp(e2))
      Bool
    case FunctionExpAnd(e1, e2) => 
      typeIs(checkExp(e1), Bool)
      typeIs(checkExp(e2), Bool)
      Bool
    case FunctionExpOr(e1, e2) =>
      typeIs(checkExp(e1), Bool)
      typeIs(checkExp(e2), Bool)
      Bool
    case FunctionExpBiImpl(e1, e2)  => 
      typeIs(checkExp(e1), Bool)
      typeIs(checkExp(e2), Bool)
      Bool
    case FunctionExpIf(c, t, e)   => 
      typeIs(checkExp(c), Bool)
      val ty = checkExp(t)
      typeIs(ty, checkExp(e))
      ty
    case FunctionExpLet(n, e, i)  => 
      val te = checkExp(e)
      withType(n, te) {(
        checkExp(i))
      }
    case FunctionExpApp(fn, args) => 
      symbolType(fn) match {
        case Some((ins, out)) => 
          args.zip(ins) foreach { ta =>
            typeIs(checkExp(ta._1), Sort(ta._2.name))
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

  
  def solveConstraints(): (USubst, Seq[Constraint]) = {
    var subst = Map[UVar, Type]()
    var unsolved = Seq[Constraint]()
    cons foreach {
      case Eq(t1, t2) => 
        t1.unify(t2, subst) match {
          case Some(ures) => subst ++= ures
          case None => unsolved :+= Eq(t1,t2)
        }
    }
    (subst, unsolved)
  }
}