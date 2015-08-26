package de.tu_darmstadt.veritas.backend.nameresolution

import org.spoofax.interpreter.core.InterpreterException
import de.tu_darmstadt.veritas.backend.util._
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.util.Context
import de.tu_darmstadt.veritas.backend.veritas.Import
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.Resolved
import de.tu_darmstadt.veritas.backend.veritas.Unresolved
import de.tu_darmstadt.veritas.backend.veritas.VeritasParseError
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl

object NameResolution {
  def getModuleDef(imp: Import): Option[Module] = imp match {
    case imp: Resolved => Some(imp.moduleCode)
    case imp: Unresolved => {
      // NOTE I added a rethrow with additional message here, because error message about "task delay ..." is very unintuitive
      val strategyResult = try {
        Context.callStrategy("get-module-ref", imp.toNablUse)
      } catch {
        case e: InterpreterException => {
          throw BackendError("Error when executing a NaBL strategy during NameResolution."
            + "\n\tPossibly because the loaded index/task engine does not match the task ID/NaBL URI annotation in the input files?"
            + "\n\tAlso, possibly because the path you gave for index and task engine persisted files does not exist or does not "
            + "contain a .cache directory with index.idx and taskengine.idx in it."
            + "\nCaused by: "
            + stacktraceToString(e))
        }
      }
      
      // FIXME Why does the strategy give us back a Result(<taskid>) instead of the module code? When does this happen? Whats the behavior per spec of get-module-ref?
      // Example test case: import of SoundnessAuxDefs.stl in sql/ProgressProjection.stl
      strategyResult match {
        case Some(StrategoAppl("Result", _)) => {
          throw BackendError("Error during NameResolution: Expected a Module() code, given the import\n" 
              + imp + "\nbut got back from the Stratego nameresolution strategy: " + strategyResult.get)
        }
        case _ => ;
      }
      
      strategyResult map (Module.from(_))
    }
  }
}
