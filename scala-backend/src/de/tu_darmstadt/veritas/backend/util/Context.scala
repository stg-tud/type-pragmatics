package de.tu_darmstadt.veritas.backend.util

import org.metaborg.runtime.task.primitives.TaskLibrary
import org.spoofax.interpreter.library.index.primitives.IndexLibrary
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.terms.TermFactory
import org.strategoxt.HybridInterpreter

import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm

/**
 * Functionality that depends on whether the Backend is run as standalone (like commandline 
 * application) or from Spoofax (called as Java strategy) is bundled here
 */
trait Context {
  def debug(x: Any): Unit
  def callStrategy(strategyName: String, termArgument: StrategoTerm): Option[StrategoTerm]
}

/**
 * Usage: before calling any method on Context, call either initStandalone() or initStrategy()
 */
// TODO mutable singletons are really bad I suppose, do I need it here?
// I need parameters to createa a StrategyContext, how do I do that with a singleton?
object Context extends Context {
  private var ctx: Context = null
  /**
   * @param indexAndTaskenginePath path to the parent of the ".cache" directory in which the 
   * "index.idx" and "taskengine.idx" files are.
   */
  def initStandalone(indexAndTaskenginePath: String): Unit = {
    ctx = new StandaloneContext(indexAndTaskenginePath)
  }
  def initStrategy(c: org.strategoxt.lang.Context): Unit = {
    ctx = new StrategyContext(c)
  }

  // forward the methods to the concrete Context instance
  override def debug(x: Any) = ctx.debug(x)
  override def callStrategy(strategyName: String, termArgument: StrategoTerm) = ctx.callStrategy(strategyName, termArgument)
}

/*
 * The two possible contexts: Running as a Strategy from Spoofax and as a Standalone application
 */

private class StandaloneContext(indexAndTaskenginePath: String) extends Context {
  override def debug(x: Any) = println(x)
  
  // use the HybridInterpreter (for the initialization, see below)
  override def callStrategy(strategyName: String, termArgument: StrategoTerm): Option[StrategoTerm] = {
    interp.setCurrent(termArgument.toIStrategoTerm)
    if (interp.invoke(strategyName)) {
      Some(StrategoTerm(interp.current))
    } else {
      None
    }
  }
  
  /* IRC help regarding interpreting *.str files
[14:39] <daehn> Hello together. Can I interpret a *.str file from a Java program (not inside Eclipse)?
[14:39] <daehn> I looked into mb-exec/.../Interpreter.java, however this seems to expect a *.ctree file as input, is this correct?
[14:53] == eriksensei [~Adium@546B12FD.cm-12-4a.dynamic.ziggo.nl] has quit [Read error: Connection reset by peer]
[14:56] == eriksensei [~Adium@546B12FD.cm-12-4a.dynamic.ziggo.nl] has joined #stratego
[15:05] <eriksensei> daehn: i'm literally in my first hour of playing with spoofax, but perhaps you could take a look at http://metaborg.org/spoofax/sunshine/?
[15:10] <dannyg> daehn: ctree file is desugared/core stratego, you can produce them by passing the -F option to the stratego compiler
[15:12] <daehn> Ah, thanks a lot! With the interpreter, it successfully loads the strategy now with interp.load("bla.ctree")
[15:12] <daehn> However, on interp.invoke("strategy_name_0_0") it throws an UndefinedStrategyException: "Not found 'is_string_0_0'"
[15:13] <daehn> Maybe I missed to load some spoofax libraries, but I thought the ctree file would contain all necessary dependencies (?)
[15:14] <dannyg> is-string is indeed defined in the standard library of stratego
[15:17] <dannyg> in spoofax there is a hybridinterpreter which loads the compiled version of the library but runs the project code interpreted
[15:17] <daehn> Ok, thanks again. The compiled version of the library sits in strategoxt.jar?
[15:18] <dannyg> yes
[15:19] <daehn> Yes, it works with HybridInterpreter! Thanks alot :)
   */

  // NOTE since the HybridInterpreter is quite slow to start up, let's delay its init until it is
  // really needed by using "lazy val"
  private lazy val interp = {
    // NOTE Mutable TermFactory is strictly requried by the TaskEngine, otherwise it gives Exception
    val termFactory = new TermFactory().getFactoryWithStorageType(IStrategoTerm.MUTABLE)
    val _interp = new HybridInterpreter(termFactory)
      // load the NaBl strategies
    _interp.load("lib/names.ctree")
    
    // also init Index
    _interp.addOperatorRegistry(new IndexLibrary())
    val language = _interp.getFactory.makeString("Veritas")
    val projectPath = _interp.getFactory.makeString(indexAndTaskenginePath)
    _interp.setCurrent(_interp.getFactory.makeTuple(language, projectPath))
    if (!_interp.invoke("index-setup")) throw BackendError("index-setup failed")
    
    // and TaskEngine
    _interp.addOperatorRegistry(new TaskLibrary())
    _interp.setCurrent(projectPath)
    if (!_interp.invoke("task-setup")) throw BackendError("task-setup failed")
    
    _interp
  }
}

private class StrategyContext(c: org.strategoxt.lang.Context) extends Context {
  override def debug(x: Any) = c.getIOAgent.printError(x.toString)

  // just use the backend strategy's context to invoke other strategies
  override def callStrategy(strategyName: String, termArgument: StrategoTerm): Option[StrategoTerm]
    = Some(StrategoTerm(c.invokeStrategy(strategyName, termArgument.toIStrategoTerm)))
}
