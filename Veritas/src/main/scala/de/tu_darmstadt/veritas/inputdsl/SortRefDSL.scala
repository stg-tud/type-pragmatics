package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.SortRef

/**
  * Created by sylvia on 19/08/16.
  */
object SortRefDSL {
  implicit def _toSortRef(s: Symbol): SortRef = SortRef(s.name)

  implicit def _toSortRefSeq(s: Symbol): Seq[SortRef] = Seq(s)


  // create a list of sort references - end point
  implicit def _toSortRefList(s: Symbol) = new _SortRefList(Seq(s))

  // create a list of sort references - end point (from SortRef)
  implicit def _toSortRefList(c: SortRef): _SortRefList = new _SortRefList(Seq(c))

  // create a list of sort references where new constructors can be added via | syntax
  implicit class _SortRefList(cons: Seq[SortRef]) {
    def -(next: SortRef): Seq[SortRef] = cons :+ next
  }

}
