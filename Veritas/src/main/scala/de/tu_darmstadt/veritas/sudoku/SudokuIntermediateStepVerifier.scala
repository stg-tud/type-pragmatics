package de.tu_darmstadt.veritas.sudoku
import z3.scala.dsl.IntVar


/**
  * verify whether an intermediate step still satisfies the rules
  */
class SudokuIntermediateStepVerifier extends SudokuLeafVerifier {


  protected def nonZeroInRow(arr: Array[Array[IntVar]],
                             f: Field, i: Int): Seq[IntVar] = {
    val varrow = arr(i)
    val fieldrow = f(i)
    for (i <- fieldrow.indices if fieldrow(i).value != 0) yield varrow(i)
  }

  protected def nonZeroInCol(arr: Array[Array[IntVar]],
                             f: Field, i: Int): Seq[IntVar] = {
    val varcol = for (r <- arr) yield r(i)
    val fieldcol = for (r <- f) yield r(i)
    for (i <- fieldcol.indices if fieldcol(i).value != 0) yield varcol(i)
  }

  protected def nonZeroInBox(arr: Array[Array[IntVar]],
                            f: Field, i: Int, boxmap: Map[Int, (Range, Range)]): Seq[IntVar] = {
    val (rowrange, colrange) = boxmap(i)
    val varrows = arr.slice(rowrange.min, rowrange.max + 1)
    val fieldrows = f.slice(rowrange.min, rowrange.max + 1)
    val varbox = (for (r <- varrows) yield r.slice(colrange.min, colrange.max + 1)).fold(Array())(_ ++ _)
    val fieldbox = (for (r <- fieldrows) yield r.slice(colrange.min, colrange.max + 1)).fold(Array())(_ ++ _)
    for (i <- fieldbox.indices if fieldbox(i).value != 0) yield varbox(i)
  }



  override def makeAndAddConstraints(spec: EmptySpec, goal: SudokuField,
                                     vars: Array[Array[IntVar]]): Unit = {
    //here we need the actual array indices
    val boxmap = goal.boxindices map {case (bi, (rowrange, colrange)) => bi - 1 -> ((rowrange.min - 1 until rowrange.max, colrange.min - 1 until colrange.min)) }

    //bounds for cell values - including 0 for empty cells
    val boundsConstr = for (r <- vars; c <- r) yield (c >= 0 && c <= goal.rownum)
    boundsConstr map (solver.assertCnstr(_))

    //constraints for given values in Sudoku - including 0 for empty cells
    val givenConstr = for (i <- 0 until goal.rownum; j <- 0 until goal.colnum)
      yield vars(i)(j) === goal.field(i)(j).value
    givenConstr map (solver.assertCnstr(_))

    //uniqueness constraints for rows, columns, boxes
    val rowConstr = for (i <- 0 until goal.rownum) yield diffConstr(nonZeroInRow(vars, goal.field, i))
    val colConstr = for (i <- 0 until goal.colnum) yield diffConstr(nonZeroInCol(vars, goal.field, i))
    val boxConstr = for (i <- 0 until boxmap.size) yield diffConstr(nonZeroInBox(vars, goal.field, i, boxmap))


    rowConstr map (solver.assertCnstr(_))
    colConstr map (solver.assertCnstr(_))
    boxConstr map (solver.assertCnstr(_))



  }

}
