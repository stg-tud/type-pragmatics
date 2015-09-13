package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct

/**
 * abstract trait for transformations of arbitrary VeritasConstructs
 * when implementing, implement the transform method
 * when applying, use apply method
 */
trait VeritasConstructTransformation {
  /**
   * translates a Veritas construct of interest into a sequence of other VeritasConstructs
   * (top-down one pass traversal!)
   */
  def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]]

  /**
   * recursively apply the transformation defined by transform
   * to a given sequence of VeritasConstructs
   * (recursively traverses the AST behind each VeritasConstruct
   */
  def apply(vcl: Seq[VeritasConstruct]): Seq[VeritasConstruct] = {   
    (for (vc <- vcl) yield {
      if (transform.isDefinedAt(vc))
        apply(transform(vc)) 
      else {
        val newchildren = (vc.children map apply)
        Seq(vc.transformChildren(newchildren))
      }
    }).flatten
  }

}