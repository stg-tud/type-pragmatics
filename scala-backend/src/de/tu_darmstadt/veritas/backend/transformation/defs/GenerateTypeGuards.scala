package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.Configuration

/**
 * For each SortDef we generate a type guard and for each ConstructorDecl we generate an axiom for the guard.
 *
 * cons D : T
 * ==>
 * axiom $T(D)
 *
 * cons E : T * U -> V
 * ==>
 * axiom $T(x), $U(y) <-> $V(E(x,y))
 *
 * Also works with Local/Strategy blocks.
 */
class GenerateTypeGuards extends ModuleTransformation {

  val ruleprefix = "guard$"

  /**
   * function that determines where and which type guards are inserted
   * override to control at which point and in which order guard axioms are inserted
   *
   * default: does not insert anything!
   */
  def insertGuardAxsHere(mdef: ModuleDef): Seq[ModuleDef] = Seq(mdef)

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    withSuper(super.transModuleDefs(mdef)) {
      case mdef => insertGuardAxsHere(mdef)
    }
  }

  def guard(name: String): String = "guard" + name

  def guardCall(sort: String, arg: FunctionExpMeta): FunctionExpApp =
    FunctionExpApp(guard(sort), Seq(arg))

  def makeGuardSignature(dataType: String): FunctionSig = {
    FunctionSig(guard(dataType), Seq(SortRef(dataType)), SortRef(DataType.Bool))
  }

  def makeGuardAxiom(name: String, in: Seq[SortRef], out: String): TypingRule = {
    val fresh = new FreshNames
    val vars = in.map(sort => FunctionMeta(MetaVar(fresh.freshName(sort.name))))

    // all vars are well-typed
    val argGuards = in.zip(vars).map {
      case (sort, v) =>
        guardCall(sort.name, v)
    }
    val argCond = FunctionExpAnd(argGuards)

    // the constructor call yields something well-typed
    val consCall = FunctionExpApp(name, vars)
    val consCond = guardCall(out, consCall)

    val consequence = FunctionExpJudgment(FunctionExpBiImpl(argCond, consCond))

    val rname = s"$ruleprefix-$out-$name"
    val rule = TypingRule(rname, Seq(), Seq(consequence))
    rule
  }
}

object GenerateAllTypeGuards extends GenerateTypeGuards {
  override def insertGuardAxsHere(mdef: ModuleDef): Seq[ModuleDef] =
    mdef match {
      case dt @ DataType(open, name, constrs) =>
        val guardFunctions = Functions(Seq(FunctionDef(makeGuardSignature(name), Seq())))
        val guardAxioms = constrs map (c => makeGuardAxiom(c.name, c.in, name))
        Seq(dt, guardFunctions, Axioms(guardAxioms))
      case c@Consts(cdseq,_) => {
        val consts_axs = cdseq map {case ConstDecl(name, sort) => 
          TypingRule(guard(name), Seq(), Seq(guardCall(sort.name, FunctionExpApp(name, Seq()))))
        }  
        Seq(c, Axioms(consts_axs))
      }
      case funs@Functions(fs) =>
        val rules = fs map { case FunctionDef(FunctionSig(name, ins, out), _) =>
          makeGuardAxiom(name, ins, out.name)
        }
        Seq(funs, Axioms(rules))
      case _ => super.insertGuardAxsHere(mdef)
    }
}

/**
 * inserts lightweight type guards (i.e. domain axiom only!) for existentially quantified variables
 * in execution goals
 */
object GenerateExecutionGuards extends GenerateTypeGuards with CollectTypes {
  override def insertGuardAxsHere(mdef: ModuleDef): Seq[ModuleDef] =
    mdef match {
      case goals @ Goals(gs, t) => {
        val axioms = for (tr <- gs) yield {
          tr match {
            case tr @ TypingRule(n, prems, conss) if (n.startsWith("execution")) => {
              findExGuardsforTypingRule(tr).distinct //rule out potential duplicates!
            }
            case _ => Seq()
          }
        }
        Seq(Local(axioms.flatten :+ goals))
      }
      case _ => super.insertGuardAxsHere(mdef)
    }

  /**
   * creates (lightweight) type guards for existentially quantified variables in goal
   */
  private def findExGuardsforTypingRule(tr: TypingRule): Seq[ModuleDef] = {
    val vars = inferMetavarTypes(tr)
    val extypes = (ex_quantifiedTypesIn(tr.premises) ++ ex_quantifiedTypesIn(tr.consequences)).distinct

    val guardmdefs = for (name <- extypes) yield {
      val guardFunctions = Functions(Seq(FunctionDef(makeGuardSignature(name), Seq())))
      val open = dataTypes(name)._1
      val constrs = dataTypes(name)._2
      if (!open) {
        val domAxioms = makeTwoDomainAxioms(name, constrs)
        Seq(guardFunctions, Axioms(domAxioms))
      } else Seq(guardFunctions)
    }
    guardmdefs.flatten
  }

  private def ex_quantifiedTypesIn(trjseq: Seq[TypingRuleJudgment]): Seq[String] = {
    (for (trj <- trjseq) yield {
      trj match {
        case ForallJudgment(vl, jdg) => ex_quantifiedTypesIn(jdg)
        case ExistsJudgment(vl, jdg) => (vl map { v => v.sortType.name }) ++ ex_quantifiedTypesIn(jdg)
        case NotJudgment(jdg)        => ex_quantifiedTypesIn(Seq(jdg))
        case OrJudgment(orc)         => orc flatMap (or => ex_quantifiedTypesIn(or))
        case _                       => Seq()
      }
    }).flatten
  }

  def makeTwoDomainAxioms(dataType: String, constrs: Seq[DataTypeConstructor]): Seq[TypingRule] = {
    val name = s"$ruleprefix-dom-$dataType"
    val v = FunctionMeta(MetaVar("X"))

    // for execution guards, provide domain axioms in both directions
    // TODO: is this really sound, for barefof and tff? (at least for tff, it should be ok)
    Seq(TypingRule(
      name + "-1",
      Seq(FunctionExpJudgment(guardCall(dataType, v))),
      Seq(OrJudgment(constrs map (c => Seq(GenerateCtorAxiomsTyped.makeEqConsFormula(c, v)))))),
      TypingRule(
      name + "-2",
      Seq(OrJudgment(constrs map (c => Seq(GenerateCtorAxiomsTyped.makeEqConsFormula(c, v))))),
      Seq(FunctionExpJudgment(guardCall(dataType, v)))))
  }

}

