package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct


/**
 * abstract trait for transformations of arbitrary VeritasConstructs
 * when implementing, implement the transform method
 * when applying, use apply method
 *
 * optionally, implement precheck if you need to check sth. for each Veritas construct
 * during a traversal or if you need to collect information about each Veritas construct
 * during traversal
 */
trait VeritasConstructTransformation {

  type A = Seq[Nothing]

  val initialAcc: A = Seq()

  def updateAcc(acc: A, vcs: Seq[VeritasConstruct]): A = initialAcc

  /**
   * translates a Veritas construct of interest into a sequence of other VeritasConstructs
   */
  def transform(vc: VeritasConstruct, acc: A = initialAcc): Option[Seq[VeritasConstruct]]

  /**
   * checks for each Veritas construct which is passed during a recursive traversal
   * whether a certain condition holds - if not, the construct is skipped during the traversal
   * (can for example be used to skip certain modules in a sequence of modules during a traversal)
   *
   * this method can also be implemented with side-effects,
   * in case e.g. some state has to be updated for each Veritas construct traversed
   */
  def precheck(vc: VeritasConstruct): Boolean = true

  /**
   * default traversal behavior of the transformation
   * standard: top-down one pass traversal until first occurrence
   * of construct to be transformed
   * (i.e. any children of the construct that would also match the
   * transform method are NOT transformed)
   *
   * if standard traversal behavior shall be changed, override def apply
   * and set it to another traversal behavior
   */
  def apply(vcl: Seq[VeritasConstruct]): Seq[VeritasConstruct] = topDownUntilFirstTraversal(vcl, initialAcc)

  /**
   * recursively apply the transformation defined by transform
   * to a given sequence of VeritasConstructs
   * (top-down one pass traversal, stops traversing as soon as a construct can be transformed via the transform function! -
   * that is, it does not check the children of such a construct again)
   */
  final def topDownUntilFirstTraversal(vcl: Seq[VeritasConstruct], acc: A = initialAcc): Seq[VeritasConstruct] = {
    (for (vc <- vcl if precheck(vc)) yield {
      transform(vc, acc) match {
        case Some(vcseq) => vcseq
        case None => {
          val newchildren = (vc.children map (cs => topDownUntilFirstTraversal(cs, updateAcc(acc, cs))))
          try {
            Seq(vc.transformChildren(newchildren))
          } catch {
            case c: ClassCastException => throw TransformationError("Child could not be transformed!")
            case e: Exception          => throw e
          }
        }
      }
    }).flatten
  }

  /**
   * recursively apply the transformation defined by transform
   * to a given sequence of VeritasConstructs
   * (top-down one pass traversal - full traversal also down the children of transformed constructs
   * careful: do not use for transformations that also return themselves again, as this will yield
   * an infinite loop)
   */
  final def topDownFullTraversal(vcl: Seq[VeritasConstruct], acc: A): Seq[VeritasConstruct] = {
    (for (vc <- vcl if precheck(vc)) yield {
      transform(vc, acc) match {
        case Some(vcseq) => topDownFullTraversal(vcseq, updateAcc(acc, vcseq))
        case None => {
          val newchildren = (vc.children map (cs => topDownFullTraversal(cs, updateAcc(acc, cs))))
          try {
            Seq(vc.transformChildren(newchildren))
          } catch {
            case c: ClassCastException => throw TransformationError("Child could not be transformed!")
            case e: Exception          => throw e
          }
        }
      }
    }).flatten
  }

  //TODO: if necessessary, implement more traversal strategies (e.g. bottom-up etc.)

}