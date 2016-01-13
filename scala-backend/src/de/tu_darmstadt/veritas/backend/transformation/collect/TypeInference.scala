package de.tu_darmstadt.veritas.backend.transformation.collect

import TypeInference._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables

trait Typeable {
  var typ: Option[TypeInference.Type] = None
  def sortType: SortRef = SortRef(typ.get.asInstanceOf[Sort].name)
  def mappingType: (Seq[SortRef], SortRef) = {
    val ft = typ.get.asInstanceOf[Function]
    val ins = ft.in map (i => SortRef(i.asInstanceOf[Sort].name))
    val out = SortRef(ft.out.asInstanceOf[Sort].name)
    (ins, out)
  }
}

object TypeInference {
  type USubst = Map[UVar, Type]

  trait Type {
    def unify(t: Type, s: USubst): Option[USubst]
    def subst(s: USubst): Type
    def occurs(t: Type): Boolean
  }
  case class UVar(name: String) extends Type {
    def unify(t: Type, s: USubst) =
      s.get(this) match {
        case Some(t1) => t1.unify(t, s)
        case None =>
          val st = t.subst(s)
          if (this == st)
            Some(s)
          else if (st.occurs(this))
            None
          else
            Some(Map(this -> st))
      }
    def subst(s: USubst) = s.getOrElse(this, this)
    def occurs(t: Type) = this == t
  }
  case class Sort(name: String) extends Type {
    def unify(t: Type, s: USubst) = t match {
      case t: UVar      => t.unify(this, s)
      case Sort(`name`) => Some(Map())
      case _            => None
    }
    def subst(s: USubst): Type = this
    def occurs(t: Type) = this == t
  }
  case class Function(in: Seq[Type], out: Type) extends Type {
    def unify(t: Type, s: USubst): Option[USubst] = t match {
      case t: UVar => t.unify(this, s)
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
    def occurs(t: Type) = (in exists (_.occurs(t))) || out.occurs(t)
  }

  val Bool = Sort("Bool")

  trait Constraint
  case class Eq(t1: Type, t2: Type) extends Constraint

  case class TypeError(msg: String) extends BackendError("Type inference failed. " + msg)
}

/**
 * Implements type inference for Veritas specifications.
 * Relies on trait `Typeable` that allows the storage of typing information in
 * AST nodes (e.g., MetaVar, FunctionExpVar, FunctionExpLet).
 *
 * TODO: Support function definitions and modules as entry points to type inference.
 */
class TypeInference(symbolType: String => Option[(Seq[SortRef], SortRef)]) {

  def inferMetavarTypes(tr: TypingRule): Seq[MetaVar] = {
    val jdgs = tr.premises ++ tr.consequences
    val vars = FreeVariables.freeVariables(jdgs).toSeq
    inferMetavarTypes(vars, jdgs)
    vars
  }

  def inferMetavarTypes(vars: Iterable[MetaVar], jdgs: Seq[TypingRuleJudgment]): Unit = {
    vars foreach (_.typ = None)
    jdgs foreach (checkJudgment(_))

    vars foreach { v =>
      if (v.typ == None)
        throw TypeError(s"Could not infer type of metavariable ${v.name}, no candidate found.")
    }

    val solveRes = solveConstraints()
    if (solveRes._2.nonEmpty)
      throw TypeError(s"Could not solve the following constraints: ${solveRes._2}")

    val subst = solveRes._1
    typeables foreach (v => v.typ = v.typ.map(_.subst(subst))) // apply subst if `v` has a type

    var res = Map[MetaVar, SortRef]()
    vars foreach { v =>
      v.typ.get match {
        case Sort(t) => res += v -> SortRef(t)
        case t: UVar => throw TypeError(s"Could not infer concrete type of metavariable $v: $t.")
        case t       => throw TypeError(s"Found complex type for metavariable $v: $t")
      }
    }
    res
  }

  private val fresh = new FreshNames

  private var cons = Seq[Constraint]()
  private def constraints = cons

  private var typeables = Seq[Typeable]()
  private var metacontext = Map[MetaVar, Type]()
  private var varcontext = Map[String, Type]()

  def typeIs(v: MetaVar, t: Type) {
    metacontext.get(v) match {
      case Some(t2) => cons :+= Eq(t, t2)
      case None     => metacontext += (v -> t)
    }
  }

  def typeIs(t1: Type, t2: Type) {
    cons :+= Eq(t1, t2)
  }

  def withType[T](v: String, t: Type)(f: => T): T = {
    val oldvarcontext = varcontext
    varcontext += (v -> t)
    val res = f
    oldvarcontext.get(v) match {
      case None    => varcontext -= v
      case Some(t) => varcontext += (v -> t)
    }
    res
  }

  def withScope(vl: Seq[MetaVar])(f: => Unit) {
    val oldmetacontext = metacontext
    metacontext --= vl
    f
    vl foreach { v =>
      v.typ = metacontext.get(v)
      typeables :+= v
      oldmetacontext.get(v) match {
        case None    => metacontext -= v
        case Some(t) => metacontext += (v -> t)
      }
    }
  }

  def freshType(v: MetaVar): Type = {
    val t = UVar(fresh.freshName("T"))
    metacontext += (v -> t)
    t
  }

  def checkJudgment(jdg: TypingRuleJudgment): Unit = jdg match {
    case FunctionExpJudgment(f)   => checkExpBool(f)
    case ExistsJudgment(vl, jdgl) => withScope(vl) { jdgl foreach (checkJudgment(_)) }
    case ForallJudgment(vl, jdgl) => withScope(vl) { jdgl foreach (checkJudgment(_)) }
    case NotJudgment(jdg)         => checkJudgment(jdg)
    case OrJudgment(jdgll)        => jdgll foreach (jdgl => jdgl foreach (checkJudgment(_)))
    case ReduceJudgment(e1, e2) =>
      checkExp(e1); checkExp(e2)
    case TypingJudgment(e1, e2, e3) =>
      checkExp(e1); checkExp(e2); checkExp(e3)
    case TypingJudgmentSimple(e1, e2) => checkExp(e1); checkExp(e2)
  }

  /**
   * This is the entry point for all expressions occurring as FunctionExpJudgment.
   */
  def checkExpBool(f: FunctionExpMeta): Type = f match {
    case FunctionMeta(m) =>
      typeIs(m, Bool)
      Bool
    case f: FunctionExp =>
      typeIs(checkExp(f), Bool)
      Bool
  }

  def checkExp(e: FunctionExpMeta): Type = e match {
    case FunctionMeta(v) =>
      val t = metacontext.get(v) match {
        case Some(t) => t
        case None    => freshType(v)
      }
      v.typ = Some(t)
      typeables :+= v
      t
    case v @ FunctionExpVar(n) =>
      varcontext.get(n) match {
        case Some(t) =>
          v.typ = Some(t)
          typeables :+= v
          t
        case None => throw TypeError(s"Cannot find type of variable symbol $n")
      }
    case FunctionExpNot(e) =>
      typeIs(checkExp(e), Bool)
      Bool
    case FunctionExpEq(e1, e2) =>
      typeIs(checkExp(e1), checkExp(e2))
      Bool
    case FunctionExpNeq(e1, e2) =>
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
    case FunctionExpBiImpl(e1, e2) =>
      typeIs(checkExp(e1), Bool)
      typeIs(checkExp(e2), Bool)
      Bool
    case FunctionExpIf(c, t, e) =>
      typeIs(checkExp(c), Bool)
      val ty = checkExp(t)
      typeIs(ty, checkExp(e))
      ty
    case v @ FunctionExpLet(n, e, i) =>
      val te = checkExp(e)
      typeables :+= v
      v.typ = Some(te)
      withType(n, te) {
        (
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
    case FunctionExpTrue =>
      Bool
    case FunctionExpFalse =>
      Bool
  }

  def solveConstraints(): (USubst, Seq[Constraint]) = {
    var subst = Map[UVar, Type]()
    var unsolved = Seq[Constraint]()
    cons foreach {
      case Eq(t1, t2) =>
        t1.unify(t2, subst) match {
          case Some(ures) => subst = (subst mapValues (_.subst(ures))) ++ ures
          case None       => unsolved :+= Eq(t1, t2)
        }
    }
    (subst, unsolved)
  }
}