package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.GoalsWithStrategy
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Lemmas
import de.tu_darmstadt.veritas.backend.veritas.LemmasWithStrategy
import de.tu_darmstadt.veritas.backend.veritas.Local
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.Strategy

/**
 * Flattens the Module structure (containing recursive ModuleDefs inside local etc. blocks) to 
 * multiple Modules. Hence, after this transformation, no Local/Strategy blocks will be present anymore.
 * Also, no GoalsWithStrategy should be present anymore, they are now desugared to Goals.
 * 
 * Expects Lemmas/LemmasWithStrategy are already desugared.
 * Should basically not change a single-goal Module (except for the name).
 * 
 * Forwards all the imports of the current module into the generated Modules. Hence, imports can
 * be resolved either before (on the "one big Module") or after the splitting (once per each 
 * generated Module).
 */

// FIXME what to do with Hide/HideAll/Include? Are they desugared/removed after this transformation also?

object SplitModulesByGoal extends ModuleTransformation {
  private var submoduleNameGenerator = new FreshNames(false)
  
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
    withSuper(super.transModule(name, is, mdefs)) {
      case Module(name, imports, mdefs) => {
        // reinitialize name generator once per Module
        submoduleNameGenerator = new FreshNames(false)
        splitRecursiveModuleDefs(mdefs, name, imports)
      }
    }

  private def splitRecursiveModuleDefs(mdefs: Seq[ModuleDef], moduleNamePrefix: String, imports: Seq[Import]): Seq[Module] = {
    var generatedSubmodules = List.empty[Module]

    // iterate from front to back, also remember all mdefs seen UP TO CURRENT:
    var mdefsUptoCurrent = List.empty[ModuleDef]
    mdefs foreach {

      /*
       * a) whenever a recursive ModuleDef "holder" is seen ("local" or "strategy") -> recursive call, append the resulting generated Modules
       */
      case Local(recMdefs) => {
        val recModuleNamePrefix = moduleNamePrefix + "-local"
        generatedSubmodules ++= splitRecursiveModuleDefs(mdefsUptoCurrent ++ recMdefs, recModuleNamePrefix, imports)
      }
      case Strategy(stratName, stratImports, recMdefs) => {
        val recModuleNamePrefix = moduleNamePrefix + "-" + stratName
        val recImports = imports ++ stratImports
        generatedSubmodules ++= splitRecursiveModuleDefs(mdefsUptoCurrent ++ recMdefs, recModuleNamePrefix, recImports)
      }
      
      /*
       * b) whenever a Goal is seen -> add new Module to output Seq
       */
      case Goals(goals, timeout) => goals map { goal =>
        generatedSubmodules :+= Module(
            submoduleNameGenerator.freshName(moduleNamePrefix + "-" + goal.name), 
            imports, 
            mdefsUptoCurrent :+ Goals(Seq(goal), timeout)
        )
      }
      // desugar to goal + referenced strategy's ModuleDefs
      case GoalsWithStrategy(strategyName, goals, timeout) => {
        // search for the referenced strategy, include its elements before the goal
        val referencedStrategy = findStrategyByName(mdefs, strategyName)
        val referencedStrategyStripped = stripGoalsAndLocals(referencedStrategy.defs)
        
        goals map { goal => 
          generatedSubmodules :+= Module(
              submoduleNameGenerator.freshName(moduleNamePrefix + "-" + goal.name), 
              // also add the strategy imports to the generated submodules
              imports ++ referencedStrategy.imports, 
              mdefsUptoCurrent ++ referencedStrategyStripped :+ Goals(Seq(goal), timeout)
          )        
        }
      }
    
      /*
       * c) Lemmas should already be desugared -> ERROR
       */
      case e: Lemmas => throw TransformationError("splitting of module failed, expected Lemmas to already be desugared but got: " + e) 
      case e: LemmasWithStrategy => throw TransformationError("splitting of module failed, expected Lemmas to already be desugared but got: " + e) 
            
      /*
       * d) all others -> pass through
       */
      case e => mdefsUptoCurrent :+= e
    }
    
    generatedSubmodules
  }

  // FIXME this only searches top-level strategies, correct?
  private def findStrategyByName(haystack: Seq[ModuleDef], strategyName: String): Strategy = 
    haystack.collectFirst{
      case s @ Strategy(n, _, _) if n == strategyName => s
    }.getOrElse(
      throw TransformationError("splitting of module failed, a GoalWithStrategy referenced strategy \"" + strategyName + "\", but couldn't find it in the Modules body"))

  private def stripGoalsAndLocals(mdefs: Seq[ModuleDef]): Seq[ModuleDef] = mdefs flatMap {
      // FIXME correct/desired to remove all locals?
      case _ : Local => None
      // recursively strip
      case Strategy(name, imports, defs) => Some(Strategy(name, imports, stripGoalsAndLocals(defs)))
      case _ : Goals => None
      case _ : GoalsWithStrategy => None
      case e => Some(e)
  }
}