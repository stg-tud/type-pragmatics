package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast.Goals
import de.tu_darmstadt.veritas.backend.ast.GoalsWithStrategy
import de.tu_darmstadt.veritas.backend.ast.Import
import de.tu_darmstadt.veritas.backend.ast.Lemmas
import de.tu_darmstadt.veritas.backend.ast.LemmasWithStrategy
import de.tu_darmstadt.veritas.backend.ast.Local
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.Strategy
import de.tu_darmstadt.veritas.backend.ast.ModuleDefHolder
import de.tu_darmstadt.veritas.backend.ast.Axioms
import de.tu_darmstadt.veritas.backend.ast.HideAll
import de.tu_darmstadt.veritas.backend.ast.Hide
import de.tu_darmstadt.veritas.backend.ast.Include
import de.tu_darmstadt.veritas.backend.ast.TypingRule

/**
 * Flattens the Module structure (containing recursive ModuleDefs inside local etc. blocks) to
 * multiple Modules. Hence, after this transformation:
 *  - no Local/Strategy blocks will be present anymore
 *  - no GoalsWithStrategy should be present anymore, they are now desugared to Goals
 *  - no Hide/HideAll/Include will be present anymore
 *
 * Expects as input a single Module where
 *  - Lemmas/LemmasWithStrategy are already desugared (to Axioms+Goals)
 *  - all Imports are already resolved (it is one self-contained Module without any imports)
 */
case class SplitModulesByGoal(filterGoals: String = "") extends ModuleTransformation {
  private var submoduleNameGenerator = new FreshNames(false)

  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] =
    withSuper(super.transModule(name, is, mdefs)) {
      case mod @ Module(name, _, defs) => {
        // reinitialize name generator whenever this transformation is called on a Module
        submoduleNameGenerator = new FreshNames(false)

        splitRecursive(mod, name, Seq(), Map(), Seq())._2
      }
    }

  /**
   * @return a pair (active ModuleDefs at the end if the processing, generated Submodules)
   */
  private def splitRecursive(holder: ModuleDefHolder,
                             moduleNamePrefix: String,
                             _allDefs: Seq[ModuleDef],
                             _strategiesWithActiveDefs: Map[Strategy, Seq[ModuleDef]],
                             _activeDefs: Seq[ModuleDef]): (Seq[ModuleDef], Seq[Module]) = {
    if (!holder.imports.isEmpty)
      throw new TransformationError("Imports must be processed (== removed and their elements included in the ModuleDefs) before this transformation, got: " + holder)

    var generatedSubmodules = Seq[Module]()

    // iterate from front to back, keep track of: 
    // (all, active or not) ModuleDefs
    var allDefs = _allDefs
    // Strategies (and the Defs at the end of their definition)
    var strategiesWithActiveDefs = _strategiesWithActiveDefs
    // "active" - that is visible in the resulting (Sub-)Module - Defs
    var activeDefs = _activeDefs

    holder.defs foreach {

      /*
       * a) whenever a recursive ModuleDef "holder" is seen ("local" or "strategy") -> recursive call, append the resulting generated Modules
       */
      case local @ Local(recMdefs) => {
        val recModuleNamePrefix = moduleNamePrefix + "-local"
        generatedSubmodules ++= splitRecursive(local, recModuleNamePrefix, allDefs, strategiesWithActiveDefs, activeDefs)._2
      }
      case strat @ Strategy(stratName, _, recMdefs) => {
        val recModuleNamePrefix = moduleNamePrefix + "-" + stratName
        val (processedStrategyDefs, newGeneratedSubmodules) = splitRecursive(strat, recModuleNamePrefix, allDefs, strategiesWithActiveDefs, activeDefs)
        generatedSubmodules ++= newGeneratedSubmodules
        // remember the strategy and the active axioms at the end its definition site (see GoalsWithStrategy)
        strategiesWithActiveDefs += (strat -> (activeDefs ++ processedStrategyDefs).distinct)
      }

      /*
       * b) HideAll/Hide/Include influence the active axiom set 
       */
      case Hide(ruleNames) => activeDefs = removeAxiomsWithNames(activeDefs, ruleNames)
      case HideAll => activeDefs = activeDefs filter {
        case _: Axioms => false
        case _         => true
      }
      case inc @ Include(axiomNames) => {
        val referencedAxioms = for (axiomName <- axiomNames) yield {
          if (!findAxiomByName(activeDefs, axiomName).isEmpty)
            throw TransformationError("Referenced axiom \"" + axiomName + "\" in include " + inc + " is already active, include is unnecessary.")

          val foundAxioms = findAxiomByName(allDefs, axiomName)
          foundAxioms match {
            case Seq()     => throw TransformationError("Referenced axiom \"" + axiomName + "\" in include " + inc + " was not found in: " + allDefs)
            case Seq(rule) => rule
            case _         => throw TransformationError("Referenced axiom \"" + axiomName + "\" in include " + inc + " was found multiple times in: " + allDefs)
          }
        }
        activeDefs +:= Axioms(referencedAxioms)
      }

      /*
       * b) whenever a Goal with the desired name is seen -> add new Module to output Seq
       */
      case Goals(goals, timeout) => goals foreach { goal =>
        if (goal.name.startsWith(filterGoals))
          generatedSubmodules :+= Module(
            submoduleNameGenerator.freshName(moduleNamePrefix + "-" + goal.name),
            Seq(),
            activeDefs :+ Goals(Seq(goal), timeout))
      }
      // desugar to goal + referenced strategy's ModuleDefs
      case GoalsWithStrategy(strategyName, goals, timeout) => {
        // search for the referenced strategy, include its elements before the goal
        // NOTE this only searches top-level strategies, but since strategies inside a local cannot
        // be referenced anyway, it is correct.
        val referencedStrategy = strategiesWithActiveDefs.keys.find {
          case Strategy(n, _, _) if n == strategyName => true
          case _                                      => false
        }.getOrElse(throw TransformationError("GoalWithStrategy referenced strategy \"" + strategyName + "\", but couldn't find it in the Modules defs seen so far: " + allDefs))

        goals foreach { goal =>
          if (goal.name.startsWith(filterGoals))
            generatedSubmodules :+= Module(
              submoduleNameGenerator.freshName(moduleNamePrefix + "-" + goal.name),
              Seq(),
              strategiesWithActiveDefs(referencedStrategy) :+ Goals(Seq(goal), timeout))
        }
      }

      /*
       * c) Lemmas should already be desugared -> ERROR
       */
      case e: Lemmas             => throw TransformationError("splitting of module failed, expected Lemmas to already be desugared but got: " + e)
      case e: LemmasWithStrategy => throw TransformationError("splitting of module failed, expected Lemmas to already be desugared but got: " + e)

      /*
       * d) all others -> pass through
       */
      case e => {
        allDefs :+= e
        activeDefs :+= e
      }
    }

    (activeDefs, generatedSubmodules)
  }

  private def findAxiomByName(haystack: Seq[ModuleDef], axiomName: String): Seq[TypingRule] = haystack flatMap {
    case holder: ModuleDefHolder => findAxiomByName(holder.defs, axiomName)
    case Axioms(typingRules)     => typingRules.filter(_.name == axiomName)
    case _                       => Seq()
  }

  private def removeAxiomsWithNames(input: Seq[ModuleDef], names: Seq[String]): Seq[ModuleDef] = input flatMap {
    // all of these are _removed_ as part of this transformation, shouldn't be present in the first place
    case _: Hide | HideAll | _: Include | _: Local | _: Strategy | _: Goals | _: GoalsWithStrategy | _: Lemmas | _: LemmasWithStrategy => throw TransformationError("Module defs when filtering for axioms should not contain Hide/HideAll/Include/Local/Strategy/Goals(WithStrategy)/Lemmas(WithStrategy))")

    case Axioms(typingRules) => Some(Axioms(typingRules filter {
      case TypingRule(ruleName, _, _) if names contains ruleName => false
      case _ => true
    }))

    case t => Some(t)
  }
}