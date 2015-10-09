package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo

/**
 * this trait tries to infer the signature of TypingJudgments from a Module
 * (i.e. tries to find types for the arguments of the typing judgment occurrences)
 *
 * assumes that within a module, typing judgments are always used with the same types
 *
 */
trait InferTypingJudgmentSignature extends ModuleTransformation with CollectTypeInfo {

  private var t1: Option[SortRef] = None
  private var t2: Option[SortRef] = None
  private var t3: Option[SortRef] = None

  var typingJudgmentFunctionDecl: Option[FunctionSig] = None

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeTypingJudgmentFunctionDecl(t1: SortRef, t2: SortRef, t3: SortRef): Unit =
    typingJudgmentFunctionDecl = Some(FunctionSig("tcheck", Seq(t1, t2, t3), SortRef("Bool")))

  /**
   * typable: toType is itself a function application or is argument of a function application
   */
  private def typeTypableOccurrence(toType: FunctionExpMeta, typableOcc: FunctionExpApp): SortRef = {
    typableOcc match {
      case FunctionExpApp(fn, args) => {
        val (in, out) = constypes.getOrElse(fn, functypes.getOrElse(fn, pfunctypes.getOrElse(fn, (Seq(), SortRef("")))))
        if (out.name == "") throw TransformationError(s"Function ${fn} was not declared!?")
        if (toType == typableOcc)
          out
        else {
          if (args contains toType)
            in(args.indexOf(toType))
          else
            throw TransformationError(s"Could not type ${toType} agains ${typableOcc}")
        }
      }
    }
  }

  /**
   * finds constructs in given sequence of TypingRuleJudgment that have the
   * given MetaVar m as direct child and are one of
   * - function applications (Appl)
   * - equalities/inequalities with the m on one side and a function application on the other side
   *
   * this function was copied from ToTff and slightly simplified (to return only FunctionExpApp occurrences)
   */
  private def findTypableOccurrences(f: FunctionExpMeta)(jdglist: Seq[TypingRuleJudgment]): Set[FunctionExpApp] = {

    def searchFunctionExp(e: FunctionExpMeta): Set[FunctionExpApp] = {
      e match {
        // base cases just return empty set, all listed explicitly just in case!  
        case FunctionMeta(m)   => Set()
        case FunctionExpVar(n) => Set()
        case FunctionExpTrue   => Set()
        case FunctionExpFalse  => Set()
        case FunctionExpNot(f) => searchFunctionExp(f)
        case fe @ FunctionExpEq(l, r @ FunctionExpApp(n, args)) =>
          if (f == l) Set(r) else searchFunctionExp(r)
        case fe @ FunctionExpEq(l @ FunctionExpApp(n, args), r) =>
          if (f == r) Set(l) else searchFunctionExp(l)
        case FunctionExpEq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case fe @ FunctionExpNeq(l, r @ FunctionExpApp(n, args)) =>
          if (f == l) Set(r) else searchFunctionExp(r)
        case fe @ FunctionExpNeq(l @ FunctionExpApp(n, args), r) =>
          if (f == r) Set(l) else searchFunctionExp(l)
        case FunctionExpNeq(f1, f2)  => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case FunctionExpAnd(l, r)    => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpOr(l, r)     => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpBiImpl(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpIf(c, t, e)  => searchFunctionExp(c) ++ searchFunctionExp(t) ++ searchFunctionExp(e)
        case FunctionExpLet(n, e, i) => searchFunctionExp(e) ++ searchFunctionExp(i)
        case fe @ FunctionExpApp(fn, args) => {
          val afe = (args flatMap searchFunctionExp).toSet
          if (args contains f) afe ++ Set(fe) else afe
        }
        // this should never happen
        case _ => throw TransformationError(s"While trying to type ${f}, found a function expression that is not covered by the code!")
      }
    }
    (for (jdg <- jdglist) yield {
      jdg match {
        case FunctionExpJudgment(f)   => searchFunctionExp(f)
        case ExistsJudgment(vl, jdgl) => findTypableOccurrences(f)(jdgl)
        case ForallJudgment(vl, jdgl) => findTypableOccurrences(f)(jdgl)
        case NotJudgment(jdg)         => findTypableOccurrences(f)(Seq(jdg))
        case OrJudgment(jdgll)        => jdgll flatMap findTypableOccurrences(f)
        case TypingJudgment(_, _, _)  => Seq() //do not search in TypingJudgments
        case _                        => throw TransformationError(s"While trying to type ${f}, encountered a construct in an axiom, premise, or conclusion that is not supported!")
      }
    }).flatten.toSet
  }

  private def findTypableOccurrence(f: FunctionExpMeta, context: VeritasConstruct): Option[FunctionExpApp] =
    context match {
      case TypingRule(n, prems, cons) => {
        val occset = findTypableOccurrences(f)(prems ++ cons)
        if (occset.size == 1)
          Some(occset.head)
        else
          None
      }
      case _ => None
      //this function only considers typing judgments that appear directly in a typing rule for typing
      //not those that might appear in exists/forall or or judgments
    }

  /**
   * tries to type a given FunctionExpMeta based on the direct context in which it occurs
   * (direct parent)
   *
   */
  private def tryTyping(f: FunctionExpMeta, context: VeritasConstruct): Option[SortRef] =
    f match {
      case fapp @ FunctionExpApp(fn, args) => try {
        Some(typeTypableOccurrence(fapp, fapp))
      } catch {
        case TransformationError(m) => throw TransformationError(s"Could not type ${f}: " + m)
      }

      case _ => try {
        val occ = findTypableOccurrence(f, context)
        if (occ == None)
          None
        else
          Some(typeTypableOccurrence(f, occ.get))
      } catch {
        case TransformationError(m) => throw TransformationError(s"Could not type ${f}: " + m)
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case t @ TypingJudgment(f1, f2, f3) => {
        //TODO maybe extract a function here
        val f1type = tryTyping(f1, path(1))
        if (f1type != None)
          t1 match {
            case None                         => t1 = f1type
            case s @ Some(_) if (s != f1type) => throw TransformationError("Found at least two different possible types for context argument of typing judgment!")
          }
        //if no type found, do nothing

        val f2type = tryTyping(f2, path(1))
        if (f2type != None)
          t2 match {
            case None                         => t2 = f2type
            case s @ Some(_) if (s != f2type) => throw TransformationError("Found at least two different possible types for program/expression argument of typing judgment!")
          }
        //if no type found, do nothing

        val f3type = tryTyping(f3, path(1))
        if (f3type != None)
          t3 match {
            case None                         => t3 = f3type
            case s @ Some(_) if (s != f3type) => throw TransformationError("Found at least two different possible types for type argument of typing judgment!")
          }
        //if no type found, do nothing

        if (typingJudgmentFunctionDecl == None && t1.isDefined && t2.isDefined && t3.isDefined)
          makeTypingJudgmentFunctionDecl(t1.get, t2.get, t3.get)
        //do not modify original typing judgment  
        t
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case t @ TypingJudgment(f1, f2, f3) => {
        //TODO maybe extract a function here
        val f1type = tryTyping(f1, path(1))
        if (f1type != None)
          t1 match {
            case None                         => t1 = f1type
            case s @ Some(_) if (s == f1type) => t1 = f1type
            case _                            => throw TransformationError("Found at least two different possible types for context argument of typing judgment!")
          }
        //if no type found, do nothing

        val f2type = tryTyping(f2, path(1))
        if (f2type != None)
          t2 match {
            case None                         => t2 = f2type
            case s @ Some(_) if (s == f2type) => t2 = f2type
            case _                            => throw TransformationError("Found at least two different possible types for program/expression argument of typing judgment!")
          }
        //if no type found, do nothing

        val f3type = tryTyping(f3, path(1))
        if (f3type != None)
          t3 match {
            case None                         => t3 = f3type
            case s @ Some(_) if (s == f3type) => t3 = f3type
            case _                            => throw TransformationError("Found at least two different possible types for type argument of typing judgment!")
          }
        //if no type found, do nothing

        if (typingJudgmentFunctionDecl == None && t1.isDefined && t2.isDefined && t3.isDefined)
          makeTypingJudgmentFunctionDecl(t1.get, t2.get, t3.get)
        //do not modify original typing judgment  
        Seq(t)
      }
    }

}

/**
 * translates occurrence of TypingJudgment into a function
 * this includes declaring this function, at the top of the module
 *
 * warning: assumes that the first usage of a TypingJudgment occurs within an Axiom (not Lemma or Goal)!
 * warning 2: also assumes that types used within arguments of a TypingJudgment are always the same
 */
trait TranslateTypingJudgment extends ModuleTransformation {

  var typingJudgmentFunctionDecl: Option[FunctionSig] = None

  class InferSignature extends InferTypingJudgmentSignature
  val inferSignature = new InferSignature

  /**
   * set to true as soon as the typingJugdment was defined once in current scope
   * (to prevent duplicate definitions)
   */
  private var tjdeclared: Boolean = false

  override def apply(m: Seq[Module]): Seq[Module] = {
    val inf = inferSignature(m)
    typingJudgmentFunctionDecl = inferSignature.typingJudgmentFunctionDecl
    inf flatMap trans
  }

  private def containsTypingJudgment(as: Seq[TypingRule]): Boolean =
    as exists { tr =>
      tr match {
        case TypingRule(n, prems, cons) =>
          (prems exists {
            case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentFunctionDecl != None && fn == typingJudgmentFunctionDecl.get.name) => true
            case _ => false
          }) ||
            (cons exists {
              case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentFunctionDecl != None && fn == typingJudgmentFunctionDecl.get.name) => true
              case _ => false
            })
      }
    }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      // just put tcheck declaration in front of axiom where it is first used
      case ax @ Axioms(as) if (typingJudgmentFunctionDecl != None && !tjdeclared && containsTypingJudgment(as)) => {
        tjdeclared = true
        Seq(Functions(Seq(FunctionDef(typingJudgmentFunctionDecl.get, Seq()))), ax)
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case TypingJudgment(f1, f2, f3) => {
        if (typingJudgmentFunctionDecl == None)
          throw TransformationError(s"Could not transform a typing judgment, no function signature could be inferred: ${trj}")
        else {
          val fn = typingJudgmentFunctionDecl.get.name
          FunctionExpJudgment(FunctionExpApp(fn, Seq(f1, f2, f3)))
        }
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case TypingJudgment(f1, f2, f3) => {
        if (typingJudgmentFunctionDecl == None)
          throw TransformationError(s"Could not transform a typing judgment, no function signature could be inferred: ${trj}")
        else {
          val fn = typingJudgmentFunctionDecl.get.name
          Seq(FunctionExpJudgment(FunctionExpApp(fn, Seq(f1, f2, f3))))
        }
      }
    }

}

object TranslateTypingJudgmentToFunction extends TranslateTypingJudgment

//below version of above trait for translating TypingJudgmentSimple (which has just two arguments)
trait InferTypingJudgmentSimple extends ModuleTransformation with CollectTypeInfo {

  protected var t1: Option[SortRef] = None
  protected var t2: Option[SortRef] = None

  var typingJudgmentFunctionDecl: Option[FunctionSig] = None

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeTypingJudgmentFunctionDecl(t1: SortRef, t2: SortRef): Unit =
    typingJudgmentFunctionDecl = Some(FunctionSig("tchecksimple", Seq(t1, t2), SortRef("Bool")))

  /**
   * typable: toType is itself a function application or is argument of a function application
   */
  private def typeTypableOccurrence(toType: FunctionExpMeta, typableOcc: FunctionExpApp): SortRef = {
    typableOcc match {
      case FunctionExpApp(fn, args) => {
        val (in, out) = constypes.getOrElse(fn, functypes.getOrElse(fn, pfunctypes.getOrElse(fn, (Seq(), SortRef("")))))
        if (out.name == "") throw TransformationError(s"Function ${fn} was not declared!?")
        if (toType == typableOcc)
          out
        else {
          if (args contains toType)
            in(args.indexOf(toType))
          else
            throw TransformationError(s"Could not type ${toType} agains ${typableOcc}")
        }
      }
    }
  }

  /**
   * finds constructs in given sequence of TypingRuleJudgment that have the
   * given MetaVar m as direct child and are one of
   * - function applications (Appl)
   * - equalities/inequalities with the m on one side and a function application on the other side
   *
   * this function was copied from ToTff and slightly simplified (to return only FunctionExpApp occurrences)
   */
  private def findTypableOccurrences(f: FunctionExpMeta)(jdglist: Seq[TypingRuleJudgment]): Set[FunctionExpApp] = {

    def searchFunctionExp(e: FunctionExpMeta): Set[FunctionExpApp] = {
      e match {
        // base cases just return empty set, all listed explicitly just in case!  
        case FunctionMeta(m)   => Set()
        case FunctionExpVar(n) => Set()
        case FunctionExpTrue   => Set()
        case FunctionExpFalse  => Set()
        case FunctionExpNot(f) => searchFunctionExp(f)
        case fe @ FunctionExpEq(l, r @ FunctionExpApp(n, args)) =>
          if (f == l) Set(r) else searchFunctionExp(r)
        case fe @ FunctionExpEq(l @ FunctionExpApp(n, args), r) =>
          if (f == r) Set(l) else searchFunctionExp(l)
        case FunctionExpEq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case fe @ FunctionExpNeq(l, r @ FunctionExpApp(n, args)) =>
          if (f == l) Set(r) else searchFunctionExp(r)
        case fe @ FunctionExpNeq(l @ FunctionExpApp(n, args), r) =>
          if (f == r) Set(l) else searchFunctionExp(l)
        case FunctionExpNeq(f1, f2)  => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case FunctionExpAnd(l, r)    => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpOr(l, r)     => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpBiImpl(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpIf(c, t, e)  => searchFunctionExp(c) ++ searchFunctionExp(t) ++ searchFunctionExp(e)
        case FunctionExpLet(n, e, i) => searchFunctionExp(e) ++ searchFunctionExp(i)
        case fe @ FunctionExpApp(fn, args) => {
          val afe = (args flatMap searchFunctionExp).toSet
          if (args contains f) afe ++ Set(fe) else afe
        }
        // this should never happen
        case _ => throw TransformationError(s"While trying to type ${f}, found a function expression that is not covered by the code!")
      }
    }
    (for (jdg <- jdglist) yield {
      jdg match {
        case FunctionExpJudgment(f)   => searchFunctionExp(f)
        case ExistsJudgment(vl, jdgl) => findTypableOccurrences(f)(jdgl)
        case ForallJudgment(vl, jdgl) => findTypableOccurrences(f)(jdgl)
        case NotJudgment(jdg)         => findTypableOccurrences(f)(Seq(jdg))
        case OrJudgment(jdgll)        => jdgll flatMap findTypableOccurrences(f)
        case TypingJudgment(_, _, _)  => Seq() //do not search in TypingJudgments
        case _                        => throw TransformationError(s"While trying to type ${f}, encountered a construct in an axiom, premise, or conclusion that is not supported!")
      }
    }).flatten.toSet
  }

  private def findTypableOccurrence(f: FunctionExpMeta, context: VeritasConstruct): Option[FunctionExpApp] =
    context match {
      case TypingRule(n, prems, cons) => {
        val occset = findTypableOccurrences(f)(prems ++ cons)
        if (occset.size == 1)
          Some(occset.head)
        else
          None
      }
      case _ => None
      //this function only considers typing judgments that appear directly in a typing rule for typing
      //not those that might appear in exists/forall or or judgments
    }

  /**
   * tries to type a given FunctionExpMeta based on the direct context in which it occurs
   * (direct parent)
   *
   */
  protected def tryTyping(f: FunctionExpMeta, context: VeritasConstruct): Option[SortRef] =
    f match {
      case fapp @ FunctionExpApp(fn, args) => try {
        Some(typeTypableOccurrence(fapp, fapp))
      } catch {
        case TransformationError(m) => throw TransformationError(s"Could not type ${f}: " + m)
      }

      case _ => try {
        val occ = findTypableOccurrence(f, context)
        if (occ == None)
          None
        else
          Some(typeTypableOccurrence(f, occ.get))
      } catch {
        case TransformationError(m) => throw TransformationError(s"Could not type ${f}: " + m)
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case t @ TypingJudgmentSimple(f1, f2) => {
        //TODO maybe extract a function here
        val f1type = tryTyping(f1, path(1))
        if (f1type != None)
          t1 match {
            case None                         => t1 = f1type
            case s @ Some(_) if (s != f1type) => throw TransformationError("Found at least two different possible types for context argument of typing judgment!")
          }
        //if no type found, do nothing

        val f2type = tryTyping(f2, path(1))
        if (f2type != None)
          t2 match {
            case None                         => t2 = f2type
            case s @ Some(_) if (s != f2type) => throw TransformationError("Found at least two different possible types for program/expression argument of typing judgment!")
          }
        //if no type found, do nothing

        if (typingJudgmentFunctionDecl == None && t1.isDefined && t2.isDefined)
          makeTypingJudgmentFunctionDecl(t1.get, t2.get)
        //do not modify original typing judgment  
        t
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case t @ TypingJudgmentSimple(f1, f2) => {
        //TODO maybe extract a function here
        val f1type = tryTyping(f1, path(1))
        if (f1type != None)
          t1 match {
            case None                         => t1 = f1type
            case s @ Some(_) if (s == f1type) => t1 = f1type
            case _                            => throw TransformationError("Found at least two different possible types for context argument of typing judgment!")
          }
        //if no type found, do nothing

        val f2type = tryTyping(f2, path(1))
        if (f2type != None)
          t2 match {
            case None                         => t2 = f2type
            case s @ Some(_) if (s == f2type) => t2 = f2type
            case _                            => throw TransformationError("Found at least two different possible types for program/expression argument of typing judgment!")
          }
        //if no type found, do nothing

        if (typingJudgmentFunctionDecl == None && t1.isDefined && t2.isDefined)
          makeTypingJudgmentFunctionDecl(t1.get, t2.get)
        //do not modify original typing judgment  
        Seq(t)
      }
    }

}

trait TranslateTypingJudgmentSimple extends ModuleTransformation {

  var typingJudgmentFunctionDecl: Option[FunctionSig] = None

  class InferSignature extends InferTypingJudgmentSignature
  val inferSignature = new InferSignature

  /**
   * set to true as soon as the typingJugdment was defined once in current scope
   * (to prevent duplicate definitions)
   */
  private var tjdeclared: Boolean = false

  override def apply(m: Seq[Module]): Seq[Module] = {
    val inf = inferSignature(m)
    typingJudgmentFunctionDecl = inferSignature.typingJudgmentFunctionDecl
    inf flatMap trans
  }

  private def containsTypingJudgment(as: Seq[TypingRule]): Boolean =
    as exists { tr =>
      tr match {
        case TypingRule(n, prems, cons) =>
          (prems exists {
            case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentFunctionDecl != None && fn == typingJudgmentFunctionDecl.get.name) => true
            case _ => false
          }) ||
            (cons exists {
              case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentFunctionDecl != None && fn == typingJudgmentFunctionDecl.get.name) => true
              case _ => false
            })
      }
    }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      // just put tchecksimple declaration in front of axiom where it is first used
      case ax @ Axioms(as) if (typingJudgmentFunctionDecl != None && !tjdeclared && containsTypingJudgment(as)) => {
        tjdeclared = true
        Seq(Functions(Seq(FunctionDef(typingJudgmentFunctionDecl.get, Seq()))), ax)
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case TypingJudgmentSimple(f1, f2) => {
        if (typingJudgmentFunctionDecl == None)
          throw TransformationError(s"Could not transform a simple typing judgment, no function signature could be inferred: ${trj}")
        else {
          val fn = typingJudgmentFunctionDecl.get.name
          FunctionExpJudgment(FunctionExpApp(fn, Seq(f1, f2)))
        }
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case TypingJudgmentSimple(f1, f2) => {
        if (typingJudgmentFunctionDecl == None)
          throw TransformationError(s"Could not transform a simple typing judgment, no function signature could be inferred: ${trj}")
        else {
          val fn = typingJudgmentFunctionDecl.get.name
          Seq(FunctionExpJudgment(FunctionExpApp(fn, Seq(f1, f2))))
        }
      }
    }

}

object TranslateTypingJudgmentSimpleToFunction extends TranslateTypingJudgmentSimple
