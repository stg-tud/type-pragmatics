package de.tu_darmstadt.veritas.backend.nameresolution

import java.io.File

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import org.metaborg.runtime.task.engine.ITaskEngine
import org.metaborg.runtime.task.engine.TaskManager
import org.spoofax.interpreter.terms.IStrategoInt
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.terms.TermFactory

import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm

/**
 * Wrapper around the Spoofax Java ITaskEngine for debugging/printing a persisted taskengine.idx file
 * (see PrintTasks object below)
 */
class Tasks(spoofaxTaskEngine: ITaskEngine) extends Iterable[(Int, StrategoTerm)] {
  // standard Map-like interface
  
  /**
   * Returns the first, solved StrategoTerm for this ID in the task engine
   */
  def get(taskId: Int): Option[StrategoTerm] = {
    val idTerm = new TermFactory().makeInt(taskId)
    for {
      task <- Option(spoofaxTaskEngine.getTask(idTerm))
      if task.solved
      result <- task.results.asScala.headOption
    } yield StrategoTerm(result)
  }
  
  
  /**
   * Iterator over the (task ID, task result) pairs */
  def iterator = {
    val solvedTaskPairs = for{
      entry <- spoofaxTaskEngine.getTaskEntries.asScala
      if entry.getValue.solved
      if entry.getKey.getTermType == IStrategoTerm.INT
      id = entry.getKey.asInstanceOf[IStrategoInt]
      result <- entry.getValue.results.asScala.headOption      
    } yield (id.intValue, StrategoTerm(result))
    solvedTaskPairs.iterator
  }
  
  // for DEBUGGING
  override def toString = this.iterator.mkString("\n")
}

// NOTE originally, I wanted to have these just as normal ctors in the class, but Scala
// does not allow any stmts (even val ...) before the this call...
object Tasks {
  /**
   * Convenience Ctor: read task engine from filename 
   */
  def fromFile(tasksFilename: String): Tasks = {
    // NOTE storageType == MUTABLE is necessary, otherwise read() fails with Exception
    val mutableTermFactory = new TermFactory().getFactoryWithStorageType(IStrategoTerm.MUTABLE)
    new Tasks(TaskManager.getInstance().read(new File(tasksFilename), mutableTermFactory))
  }

  /**
   * Convenience Ctor: use the current task engine (useful when executing from Spoofax)
   */
  def current(): Tasks = new Tasks(TaskManager.getInstance().getCurrent())
}

object PrintTasks extends App {
  println(Tasks.fromFile("test/.cache/taskengine.idx"))
}