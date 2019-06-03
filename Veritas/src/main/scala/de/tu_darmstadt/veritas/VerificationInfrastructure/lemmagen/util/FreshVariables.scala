package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef}
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.backend.util.FreshNames

/** Generate descriptive fresh variable symbols that refer to the respective type of the variable.
  *
  * e.g. for the type TType, generate fresh variable symbols tt, tt1, tt2, ...
  * for a type RawTable, generate fresh variable symbols rt, rt1, rt2, ...
  */
object FreshVariables {
  def freshName(freeVariables: Set[MetaVar], typ: SortRef): String = {
    // get upper-cased characters of type name
    val upperPortion = typ.name.filter(_.isUpper)
    val prefix = if (upperPortion.isEmpty) "v" else upperPortion.toLowerCase
    freshName(freeVariables, prefix)
  }

  def freshName(freeVariables: Set[MetaVar], prefix: String): String = {
    val freshNames = new FreshNames(false)
    var newName = ""
    do {
      newName = freshNames.freshName(prefix)
    } while (freeVariables.exists(_.name == newName)) // TODO: might need to check for constants?
    newName
  }

  /** Generate a fresh MetaVar instance of sort `typ`, avoiding clashes with `freeVariables` */
  def freshMetaVar(freeVariables: Set[MetaVar], typ: SortRef): MetaVar = {
    val mv = MetaVar(freshName(freeVariables, typ))
    mv.typ = Some(TypeInference.Sort(typ.name))
    mv
  }

  /** Generate a sequence of fresh MetaVar instances according to `types`,
    * avoiding clashes with `freeVariables` */
  def freshMetaVars(freeVariables: Set[MetaVar], types: Seq[SortRef]): Seq[MetaVar] = {
    var newFreeVariables = freeVariables
    types.map(typ => {
      val mv = freshMetaVar(newFreeVariables, typ)
      newFreeVariables += mv
      mv
    })
  }
}
