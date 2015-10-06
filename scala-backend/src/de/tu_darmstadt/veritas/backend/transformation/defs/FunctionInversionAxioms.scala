package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames

/**
 * generates inversion axioms for functions and partial functions
 * assumes that function equations have already been transformed to axioms!
 * assumes that all equations for one function are contained in a single Axioms-block
 * (uses the discovered function axioms for generating the inversion lemmas!)
 *
 */
trait FunctionInversionAxioms extends ModuleTransformation with CollectTypeInfo {
  val freshNames = new FreshNames

  /**
   * saves the signature of the function for which an inversion axiom is to be generated
   */
  var currfs: (String, (Seq[SortRef], SortRef)) = ("", (Seq(), SortRef("")))

  /**
   * saves variable names for premise
   */
  var genNames: (Seq[MetaVar], MetaVar) = (Seq(), MetaVar(""))

  /**
   * override to control for which functions inversion axioms are generated!
   * default: all! (even partial functions)
   *
   * parameter: name of the function (overriding classes can use functypes/pfunctypes to check what function it is!)
   */
  def checkFunction(fn: String): Boolean = true

  /**
   * override to control the names of variables in premises
   * default: sort names for function arguments, result for function result
   *
   * parameter: none, uses currfs to generate names (sets genNames)
   */
  def newMetaVars(): Unit =
    currfs match {
      case (_, (pars, res)) =>
        genNames = (for (sr <- pars) yield MetaVar(freshNames.freshName(sr.name)), MetaVar("RESULT"))
    }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case as @ Axioms(tseq) if (isFunctionDef(tseq)) => Seq(as, generateInversionAxiom(tseq))
    }

  /**
   * assumption: if all axioms in a block start with a function name, then these axioms belong to
   * the same function definition
   */
  private def isFunctionDef(tseq: Seq[TypingRule]): Boolean = {
    val rulenames = for (tr <- tseq) yield tr match {
      case TypingRule(n, _, _) => n
    }

    val func = for { fn <- functypes.keys if (rulenames.forall { rn => rn.startsWith(fn) }) } yield fn

    func match {
      case Seq()                          => false
      case Seq(fn) if (checkFunction(fn)) => { currfs = (fn, functypes(fn)); newMetaVars(); true }
      case _                              => false
    }
  }

  private def generateInversionAxiom(tseq: Seq[TypingRule]): Axioms = {
    val name = currfs._1
    val params = currfs._2._1
    val res = currfs._2._2
    if (res.name != "Bool") {
      val orcases = for (ts <- tseq) yield makeInvCase(ts)
      val conc = if (orcases.size > 1) Seq(OrJudgment(orcases)) else orcases.head
      Axioms(Seq(TypingRule(name + "-INV", Seq(makeInvFunPremise()), conc)))
    } else {
      val truerules = filterTrue(tseq)
      val truecases = for (ts <- truerules) yield makeInvTrueCase(ts)
      val tconc = if (truecases.size > 1) Seq(OrJudgment(truecases)) else truecases.head
      val falserules = filterFalse(tseq)
      val falsecases = for (ts <- falserules) yield makeInvFalseCase(ts)
      val fconc = if (falsecases.size > 1) Seq(OrJudgment(falsecases)) else falsecases.head
      Axioms(Seq(TypingRule(name + "-true-INV", Seq(makeInvTruePremise()), tconc),
        TypingRule(name + "-false-INV", Seq(makeInvFalsePremise()), fconc)))
    }
  }

  private def makeInvTruePremise(): FunctionExpJudgment =
    FunctionExpJudgment(FunctionExpApp(currfs._1, genNames._1 map { m => FunctionMeta(m) }))

  private def makeInvFalsePremise(): FunctionExpJudgment =
    FunctionExpJudgment(FunctionExpNot(FunctionExpApp(currfs._1, genNames._1 map { m => FunctionMeta(m) })))

  private def makeInvFunPremise(): FunctionExpJudgment =
    FunctionExpJudgment(FunctionExpEq(FunctionExpApp(currfs._1, genNames._1 map { m => FunctionMeta(m) }),
      FunctionMeta(genNames._2)))

  private def filterTrue(tseq: Seq[TypingRule]): Seq[TypingRule] =
    tseq filter {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpApp(fn, _)))) if (fn == currfs._1) => true
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), _)))) if (fn == currfs._1) => true
      case _ => false
    }

  private def filterFalse(tseq: Seq[TypingRule]): Seq[TypingRule] =
    tseq filter {
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpNot(FunctionExpApp(fn, _))))) if (fn == currfs._1) => true
      case TypingRule(_, _, Seq(FunctionExpJudgment(FunctionExpBiImpl(FunctionExpApp(fn, _), _)))) if (fn == currfs._1) => true
      case _ => false
    }

  private def makeInvCase(ts: TypingRule): Seq[TypingRuleJudgment] = ???

  private def makeInvTrueCase(ts: TypingRule): Seq[TypingRuleJudgment] = ???

  private def makeInvFalseCase(ts: TypingRule): Seq[TypingRuleJudgment] = ???

}