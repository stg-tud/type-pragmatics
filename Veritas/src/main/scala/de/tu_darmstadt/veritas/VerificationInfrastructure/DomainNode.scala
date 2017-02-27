package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver.Context

trait DomainEdgeLabel

object NoInfoDomainEdgeLabel extends DomainEdgeLabel


/**
  * S: format of specification
  * V: format for variable names (e.g. String)
  */
abstract class DomainNode[S, V](val spec: S, val instantiation: Map[V, S]) {

  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  def updateInstantiation(instantiation: Map[V, S]): DomainNode[S, V]

  def prettyPrint(): String

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  def instantiatedSpec(): S

}

case class ExpressionDomainNode[S, V](override val spec: S, override val instantiation: Map[V, S])
  extends DomainNode[S, V](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[V, S]): DomainNode[S, V] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): S = ???
}

case class TypeDomainNode[S, V](override val spec: S, override val instantiation: Map[V, S])
  extends DomainNode[S, V](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[V, S]): DomainNode[S, V] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): S = ???
}

/**
  *
  * @param spec
  * @param instantiation
  * @param exprargs
  * @param exprresult
  * @param canfail
  * @param acceptedfailure
  * @param transforms
  * @param successcondition
  * @tparam S
  * @tparam V
  * @tparam P format of properties
  */
case class DynamicDomainNode[S, V, P](override val spec: S,
                                          override val instantiation: Map[V, S],
                                          val exprargs: Seq[FocusedExpDomTree[S, V]],
                                          val exprresult: FocusedExpDomTree[S, V],
                                          val canfail: Boolean,
                                          val acceptedfailure: FocusedExpDomTree[S, V],
                                          val transforms: Option[V],
                                          val successcondition: P)
  extends DomainNode[S, V](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[V, S]): DomainNode[S, V] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): S = ???
}

case class StaticDomainNode[S, V, P](override val spec: S,
                                         override val instantiation: Map[V, S],
                                         val exprargs: Seq[FocusedExpDomTree[S, V]],
                                         val typargs: Seq[FocusedTypDomTree[S, V]],
                                         val successcondition: P)

  extends DomainNode[S, V](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[V, S]): DomainNode[S, V] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): S = ???
}