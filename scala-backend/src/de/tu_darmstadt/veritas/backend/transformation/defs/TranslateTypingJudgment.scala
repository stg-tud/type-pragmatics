package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError


trait InferTypingJudgmentSignature extends ModuleTransformation {
  
  private var t1: Option[SortRef] = None 
  private var t2: Option[SortRef] = None 
  private var t3: Option[SortRef] = None 
  
  protected var typingJudgmentFunctionDecl: Option[FunctionSig] = None
  
  /**
   * override this function to change the function signature generated for the typing judgment
   */
  def makeTypingJudgmentFunctionDecl(t1: SortRef, t2: SortRef, t3: SortRef): Unit = 
      typingJudgmentFunctionDecl = Some(FunctionSig("tcheck", Seq(t1, t2, t3), SortRef("Bool")))
    
      
  private def tryTyping(f: FunctionExpMeta, context: VeritasConstruct): Unit = ???
    
  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
    case t@ TypingJudgment(f1, f2, f3) => {
      tryTyping(f1, path(0))
      tryTyping(f2, path(0))
      tryTyping(f3, path(0))
      if (typingJudgmentFunctionDecl == None && t1.isDefined && t2.isDefined && t3.isDefined)
        makeTypingJudgmentFunctionDecl(t1.get, t2.get, t3.get)
        
      t
    }
  }

  
}

/**
 * translates occurrence of TypingJudgment into a function
 * (including a declaration of that function, with type inference)
 * 
 * warning: assumes that the first usage of a TypingJudgment occurs within an Axiom (not Lemma or Goal)!
 * warning 2: also assumes that types used within arguments of a TypingJudgment are always the same
 * (uses first occurrence of TypingJudgment for type inference!)
 */
trait TranslateTypingJudgment extends ModuleTransformation with InferTypingJudgmentSignature {

  /**
   * set to true as soon as the typingJugdment was defined once in current scope
   * (to prevent duplicate definitions)
   */
  private var tjdeclared: Boolean = false

  
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = {
    val oldtjdeclared = tjdeclared
    withSuper(super.transModuleDefs(mdef)) {
      //Note that TypingJudgments in as have already been transformed to function via the super call!
      case ax @ Axioms(as) => {
        if (typingJudgmentFunctionDecl != None && !tjdeclared) {
          tjdeclared = true
          Seq(Functions(Seq(FunctionDef(typingJudgmentFunctionDecl.get, Seq()))), ax)
        } else
          Seq(ax)
      }
      case l @ Local(_)          => { tjdeclared = oldtjdeclared; Seq(l) }
      case s @ Strategy(_, _, _) => { tjdeclared = oldtjdeclared; Seq(s) }
    }
  }
  
  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
    case TypingJudgment(f1, f2, f3) => ???
  }

}