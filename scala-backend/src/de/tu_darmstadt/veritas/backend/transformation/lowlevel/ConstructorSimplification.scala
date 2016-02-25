package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.util._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectConstructorNames

object ConstructorSimplification extends ModuleTransformation with CollectConstructorNames {

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = 
    mdef match {
      case Axioms(as) => 
        Seq(Axioms(trace(as){tr =>
          if (tr.name.startsWith("DIFF-") || tr.name.startsWith("EQ-"))
            Seq(tr)
          else
            transTypingRules(tr)
        }))
      case _ => super.transModuleDefs(mdef)
    }

  
  
  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper[FunctionExp](super.transFunctionExps(f)) {case x => Seq(transFunctionExp(x))}

  override def transFunctionExp(f: FunctionExp): FunctionExp = 
    withSuper(super.transFunctionExp(f)) {
      case FunctionExpEq(FunctionExpApp(c1, as), FunctionExpApp(c2, bs)) 
        if c1 == c2 && consNames.contains(c1) && as.size == bs.size 
        => FunctionExpAnd(as.zip(bs).map(ab => FunctionExpEq(ab._1,ab._2)))

      case FunctionExpEq(FunctionExpApp(c1, as), FunctionExpApp(c2, bs)) 
        if c1 != c2 && consNames.contains(c1) && consNames.contains(c2) 
        => FunctionExpFalse

      case FunctionExpNeq(FunctionExpApp(c1, as), FunctionExpApp(c2, bs)) 
        if c1 == c2 && consNames.contains(c1) && as.size == bs.size 
        => FunctionExpOr(as.zip(bs).map(ab => FunctionExpNeq(ab._1,ab._2)))

      case FunctionExpNeq(FunctionExpApp(c1, as), FunctionExpApp(c2, bs))
        if c1 != c2 && consNames.contains(c1) && consNames.contains(c2)
        => FunctionExpTrue
    }

}