package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver.Context

trait DomainEdgeLabel

object NoInfoDomainEdgeLabel extends DomainEdgeLabel

trait ExpressionDomainEdgeLabel extends DomainEdgeLabel

case class DTConstructor[I](parentname: I) extends ExpressionDomainEdgeLabel

case class DTConstructorArg[I](argname: I) extends ExpressionDomainEdgeLabel

case class DTConstructorRecArg[I](argname: I, relnodename: String) extends ExpressionDomainEdgeLabel

/**
  *
  * @param spec specification construct that describes the domain node
  * @param instantiation
  * @tparam C type of single specification construct
  * @tparam I type of identifier names
  */
abstract class DomainNode[C, I](val spec: C, val instantiation: Map[I, C]) {

  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  def updateInstantiation(instantiation: Map[I, C]): DomainNode[C, I]

  def prettyPrint(): String

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  def instantiatedSpec(): C

}

case class ExpressionDomainNode[C, I](override val spec: C, override val instantiation: Map[I, C])
  extends DomainNode[C, I](spec, instantiation) {

  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[I, C]): DomainNode[C, I] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): C = ???
}

case class TypeDomainNode[C, I](override val spec: C, override val instantiation: Map[I, C])
  extends DomainNode[C, I](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[I, C]): DomainNode[C, I] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): C = ???
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
  * @tparam C
  * @tparam I
  * @tparam P format of properties
  */
case class DynamicDomainNode[C, I, P](override val spec: C,
                                          override val instantiation: Map[I, C],
                                          val exprargs: Seq[FocusedExpDomTree[C, I]],
                                          val exprresult: FocusedExpDomTree[C, I],
                                          val canfail: Boolean,
                                          val acceptedfailure: FocusedExpDomTree[C, I],
                                          val transforms: Option[I],
                                          val successcondition: P)
  extends DomainNode[C, I](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[I, C]): DomainNode[C, I] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): C = ???
}

case class StaticDomainNode[C, I, P](override val spec: C,
                                         override val instantiation: Map[I, C],
                                         val exprargs: Seq[FocusedExpDomTree[C, I]],
                                         val typargs: Seq[FocusedTypDomTree[C, I]],
                                         val successcondition: P)

  extends DomainNode[C, I](spec, instantiation) {
  /**
    * nodes can be instantiated
    *
    * @param instantiation changed instantiation of node variables
    * @return node with updated instantiation map
    */
  override def updateInstantiation(instantiation: Map[I, C]): DomainNode[C, I] = ???

  override def prettyPrint(): String = ???

  /**
    * return the spec with all instantiations filled in
    * implementation depends on concrete format S!
    *
    * @return
    */
  override def instantiatedSpec(): C = ???
}