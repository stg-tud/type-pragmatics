package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.TacticApplicationException
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer}
import de.tu_darmstadt.veritas.sudoku._

/**
  * Domain-specific tactic for ruling out candidates according to the Sudoku strategies Naked Pair/Triple/Quad...
  *
  * This tactic assumes that all candidates are correctly entered and that all hidden singles have already been solved
  *
  * @param n : size of group of candidates, i.e. 2 for naked pair, 3 for naked triple....
  */
class NakedGroup(n: Int) extends SudokuTactic {

  //given a cell and a group of its peers, find empty cells where size of union of candidates <= n
  def findInPeers(cell: SudokuCell, peers: IndexedSudokuUnit): IndexedSudokuUnit =
    peers.filter { case (pos, c) => c.value == 0 && (cell.candidates union c.candidates).size <= n }

  //compute candidate union for a given group of cells
  def getCandidateSetUnion(cellgroup: IndexedSudokuUnit): Set[Int] =
    cellgroup.foldLeft[Set[Int]](Set())((acc, icell) => acc union icell._2.candidates)

  //find first group of size n in peers of a given cell where the size of the union of the candidates = n
  def sameNCandidates(sudokuField: SudokuField)(cell: IndexedCell): Option[IndexedSudokuUnit] = {
    val cellpos = cell._1
    val cellcont = cell._2
    lazy val peerlist = List(sudokuField.rowPeers(Seq(cellpos)),
      sudokuField.colPeers(Seq(cellpos)),
      sudokuField.boxPeers(Seq(cellpos)))

    lazy val grouplist = for (p <- peerlist;
                              group = cell +: findInPeers(cellcont, p)
                              if group.size == n && getCandidateSetUnion(group).size == n) yield group

    grouplist.headOption
  }

  //given an already computed naked group, compute the set of updated cells (where candidates from cellgroup are removed)
  def updatedCells(sudokuField: SudokuField)(cellgroup: IndexedSudokuUnit): IndexedSudokuUnit = {
    //compute all peers from the group just computed, focus on empty ones
    val groupPeers: IndexedSudokuUnit = sudokuField.allPeers(cellgroup map (_._1))
    val emptygroupPeers: IndexedSudokuUnit = sudokuField.filterCells(groupPeers, c => c.value == 0)

    val ruleout = getCandidateSetUnion(cellgroup)

    for ((pos, c) <- emptygroupPeers; newcand = c.candidates diff ruleout; if newcand != c.candidates)
      yield (pos, SudokuCell(0, newcand))

  }

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  override def apply[Obligation](obl: GenObligation[EmptySpec, SudokuField],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[EmptySpec, SudokuField, Obligation]): Iterable[(Obligation, EdgeLabel)] = {

    val sudokuField = obl.goal
    //1) find groups of n cells where the size of the union of the candidates also equals n and which share at least one unit
    //first focus on cells whose number of candidates <= n
    val cellsWithMaxNCand: IndexedSudokuUnit = sudokuField.filterCells(sudokuField.indexedcells,
      c => c.value == 0 && c.candidates.size <= n)

    //lazily compute groups that satisfy the condition
    lazy val grouplist = for (c <- cellsWithMaxNCand;
                              candidateGroup = sameNCandidates(sudokuField)(c)
                              if candidateGroup.isDefined) yield candidateGroup.get

    if (grouplist.isEmpty)
      throw NoNewNakedGroupOfSizeNFound(n)
    else {

      lazy val updateCells: Map[IndexedSudokuUnit, IndexedSudokuUnit] =
        (for (g <- grouplist;
              updCells = updatedCells(sudokuField)(g)
              if updCells.nonEmpty)
          yield g -> updCells).toMap

      if (updateCells.isEmpty)
        throw NoNewNakedGroupOfSizeNFound(n)
      else {
        val (group, upd) = updateCells.head
        val newfield = sudokuField.updateSudokuField(upd)
        val newedge = FoundNakedGroup(n, group, upd)

        Seq((produce.newObligation(obl.spec, newfield), newedge))
      }

    }

  }

  override def toString: String = s"Searching naked group of size $n"
}
