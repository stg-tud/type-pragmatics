package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.Constructors
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.TypingRule
import de.tu_darmstadt.veritas.backend.veritas.TypingRuleJudgment
import de.tu_darmstadt.veritas.backend.veritas.ForallJudgment
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpApp
import de.tu_darmstadt.veritas.backend.veritas.MetaVar
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpEq
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpBiImpl
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpAnd

import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment.wrap

object GenerateCtorAxioms extends ModuleDefTransformation {
  override protected def apply(input: ModuleDef): ModuleDef = input match {
    case Constructors(decls) => {
      var generatedAxioms: Seq[TypingRule] = Nil
      
      // generate EQ axioms
      for (ctorDecl <- decls) {
        val args = ctorDecl.in.map(_.name)
        // FIXME this is a hacky "give me fresh variables"
        val argsLeft = (args, Range(0, args.length)).zipped.map(_ + _).map(MetaVar(_)).map(FunctionExpMeta(_))
        val argsRight = (args, Range(args.length, 2*args.length)).zipped.map(_ + _).map(MetaVar(_)).map(FunctionExpMeta(_))
        
        // no premise, just a biimplication as conclusion
        generatedAxioms +:= TypingRule("EQ-" + ctorDecl.name, Nil, 
          Seq(FunctionExpBiImpl(
            FunctionExpAnd((argsLeft, argsRight).zipped map (FunctionExpEq(_, _))),
            FunctionExpEq(FunctionExpApp(ctorDecl.name, argsLeft),
                          FunctionExpApp(ctorDecl.name, argsRight))
          )))
      }
      
      // generate DIFF axioms
      // TODO
      
      Axioms(generatedAxioms)
    }
    
    // do not change anything but ctors
    case _ => input
  }
}