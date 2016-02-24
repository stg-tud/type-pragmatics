package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes
import de.tu_darmstadt.veritas.backend.util.BackendError

/**
 * trait for inferring the types of the arguments of the different typing judgments
 */
class InferTypingJudgmentsSignature extends CollectTypes {
  //types of 3-argument typing judgment
  val targs: Array[Option[SortRef]] = Array(None, None, None)

  //types of 2-argument typing judgment
  val stargs: Array[Option[SortRef]] = Array(None, None)

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] =
    tr match {
      case TypingRule(n, prems, conss) => {
        try {
          inferMetavarTypes(tr) //try to infer types for all meta vars
        } catch {
          case _: BackendError[_] => ; //ignore any inference errors for now
        }
        Seq(TypingRule(n, trace(prems)(transTypingRuleJudgments(_)), trace(conss)(transTypingRuleJudgments(_))))
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case t @ TypingJudgment(f1, f2, f3)    => { updateTargs(Seq(f1, f2, f3)); t }
      case st @ TypingJudgmentSimple(f1, f2) => { updateStargs(Seq(f1, f2)); st }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case t @ TypingJudgment(f1, f2, f3)    => { updateTargs(Seq(f1, f2, f3)); Seq(t) }
      case st @ TypingJudgmentSimple(f1, f2) => { updateStargs(Seq(f1, f2)); Seq(st) }
    }

  private def inferSingleArg(f: FunctionExpMeta): Option[SortRef] =
    f match {
      case FunctionMeta(m) => try {
        Some(m.sortType)
      } catch {
        case e: java.util.NoSuchElementException => None
        case e: java.lang.ClassCastException     => None
      }
      case FunctionExpApp(f, args) => {
        val (argstype, restype): (Seq[SortRef], SortRef) =
          functypes.getOrElse(f,
            pfunctypes.getOrElse(f,
              constrTypes.getOrElse(f, (Seq(), SortRef("")))))
        if (restype.name.isEmpty())
          None
        else
          Some(restype)
      }
      case _ => throw BackendError("An argument of a typing rule judgment was unequal to a meta-variable or an application: " + f)
    }

  //TODO is there any nice way to remove this code duplication?
  private def updateTargs(args: Seq[FunctionExpMeta]) = {
    for (i <- 0 until 3) {
      val argtype = inferSingleArg(args(i))
      argtype match {
        case Some(t) if (targs(i) == None)  => targs(i) = Some(t)
        case Some(t) if (targs(i).get == t) => ;
        case None                           => ;
        case _                              => throw BackendError("Found two different types for arguments of typing judgment - cannot infer type signature of typing judgment!")
      }
    }
  }

  private def updateStargs(args: Seq[FunctionExpMeta]) = {
    for (i <- 0 until 2) {
      val argtype = inferSingleArg(args(i))
      argtype match {
        case Some(t) if (stargs(i) == None)  => stargs(i) = Some(t)
        case Some(t) if (stargs(i).get == t) => ;
        case None                            => ;
        case _                               => throw BackendError("Found two different types for arguments of simple typing judgment - cannot infer type signature of simple typing judgment!")
      }
    }
  }

}

/**
 * trait for translating different typing judgments to a function or a predicate
 * (depending on the current configuration)
 */
trait TranslateTypingJudgments extends ModuleTransformation {

  var typingJudgmentSignature: Option[FunctionSig] = None
  var stypingJudgmentSignature: Option[FunctionSig] = None

  var makeTJFunctionExp: ((FunctionExpMeta, FunctionExpMeta, FunctionExpMeta) => FunctionExp) = (a, b, c) => FunctionExpTrue
  var makeSTJFunctionExp: ((FunctionExpMeta, FunctionExpMeta) => FunctionExp) = (a, b) => FunctionExpTrue

  /**
   * set to true as soon as the typing judgment was defined once in current scope
   * (to prevent duplicate definitions)
   */
  private var tjdeclared: Boolean = false

  /**
   * set to true as soon as the simple typing judgment was defined once in current scope
   * (to prevent duplicate definitions)
   */
  private var stjdeclared: Boolean = false

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    tjdeclared = false
    stjdeclared = false
    val inferinst = new InferTypingJudgmentsSignature
    inferinst(m)

    makeTypingJudgmentPredicateDecl(inferinst.targs(0).getOrElse(SortRef("Ctx")), inferinst.targs(1).getOrElse(SortRef("Exp")), inferinst.targs(2).getOrElse(SortRef("Typ")))
    makeStypingJudgmentPredicateDecl(inferinst.stargs(0).getOrElse(SortRef("Exp")), inferinst.stargs(1).getOrElse(SortRef("Typ")))
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      // just put typing judgment declaration in front of axiom where it is first used
      case ax @ Axioms(as) if (typingJudgmentSignature != None && !tjdeclared && containsTypingJudgment(as)) => {
        tjdeclared = true
        Seq(Functions(Seq(FunctionDef(typingJudgmentSignature.get, Seq()))), ax)
      }
      // just put typing simple judgment declaration in front of axiom where it is first used
      case ax @ Axioms(as) if (stypingJudgmentSignature != None && !stjdeclared && containsSTypingJudgment(as)) => {
        stjdeclared = true
        Seq(Functions(Seq(FunctionDef(stypingJudgmentSignature.get, Seq()))), ax)
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case TypingJudgment(f1, f2, f3) => {
        if (typingJudgmentSignature == None)
          throw TransformationError(s"Could not transform a typing judgment, no function signature could be inferred: ${trj}")
        else {
          FunctionExpJudgment(makeTJFunctionExp(f1, f2, f3))
        }
      }
      case TypingJudgmentSimple(f1, f2) => {
        if (stypingJudgmentSignature == None)
          throw TransformationError(s"Could not transform a simple typing judgment, no function signature could be inferred: ${trj}")
        else {
          FunctionExpJudgment(makeSTJFunctionExp(f1, f2))
        }
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case TypingJudgment(f1, f2, f3) => {
        if (typingJudgmentSignature == None)
          throw TransformationError(s"Could not transform a typing judgment, no function signature could be inferred: ${trj}")
        else {
          Seq(FunctionExpJudgment(makeTJFunctionExp(f1, f2, f3)))
        }
      }
      case TypingJudgmentSimple(f1, f2) => {
        if (stypingJudgmentSignature == None)
          throw TransformationError(s"Could not transform a simple typing judgment, no function signature could be inferred: ${trj}")
        else {
          Seq(FunctionExpJudgment(makeSTJFunctionExp(f1, f2)))
        }
      }
    }

  private def containsTypingJudgment(as: Seq[TypingRule]): Boolean =
    as exists { tr =>
      tr match {
        case TypingRule(n, prems, cons) =>
          (prems exists {
            case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
            case FunctionExpJudgment(FunctionExpEq(FunctionExpApp(fn, _), _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
            case _ => false
          }) ||
            (cons exists {
              case FunctionExpJudgment(FunctionExpApp(fn, _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
              case FunctionExpJudgment(FunctionExpEq(FunctionExpApp(fn, _), _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
              case _ => false
            })
      }
    }

  private def containsSTypingJudgment(as: Seq[TypingRule]): Boolean =
    as exists { tr =>
      tr match {
        case TypingRule(n, prems, cons) =>
          (prems exists {
            case FunctionExpJudgment(FunctionExpApp(fn, _)) if (stypingJudgmentSignature != None && fn == stypingJudgmentSignature.get.name) => true
            case FunctionExpJudgment(FunctionExpEq(FunctionExpApp(fn, _), _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
            case _ => false
          }) ||
            (cons exists {
              case FunctionExpJudgment(FunctionExpApp(fn, _)) if (stypingJudgmentSignature != None && fn == stypingJudgmentSignature.get.name) => true
              case FunctionExpJudgment(FunctionExpEq(FunctionExpApp(fn, _), _)) if (typingJudgmentSignature != None && fn == typingJudgmentSignature.get.name) => true
              case _ => false
            })
      }
    }

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeTypingJudgmentFunctionDecl(t1: SortRef, t2: SortRef, t3: SortRef): Unit = {
    typingJudgmentSignature = Some(FunctionSig("tcheck", Seq(t1, t2), t3))
    makeTJFunctionExp = (a1, a2, a3) => FunctionExpEq(FunctionExpApp("tcheck", Seq(a1, a2)), a3)
  }

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeStypingJudgmentFunctionDecl(t1: SortRef, t2: SortRef): Unit = {
    stypingJudgmentSignature = Some(FunctionSig("tchecksimple", Seq(t1), t2))
    makeSTJFunctionExp = (a1, a2) => FunctionExpEq(FunctionExpApp("tchecksimple", Seq(a1)), a2)
  }

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeTypingJudgmentPredicateDecl(t1: SortRef, t2: SortRef, t3: SortRef): Unit = {
    typingJudgmentSignature = Some(FunctionSig("ptcheck", Seq(t1, t2, t3), SortRef("Bool")))
    makeTJFunctionExp = (a1, a2, a3) => FunctionExpApp("ptcheck", Seq(a1, a2, a3))
  }

  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeStypingJudgmentPredicateDecl(t1: SortRef, t2: SortRef): Unit = {
    stypingJudgmentSignature = Some(FunctionSig("ptchecksimple", Seq(t1, t2), SortRef("Bool")))
    makeSTJFunctionExp = (a1, a2) => FunctionExpApp("ptchecksimple", Seq(a1, a2))
  }
}

object TranslateAllTypingJudgments extends TranslateTypingJudgments
