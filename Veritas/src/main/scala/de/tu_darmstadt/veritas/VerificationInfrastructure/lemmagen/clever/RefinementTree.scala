package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class RefinementTree[Tag](rootLemma: Lemma, rootTag: Tag) {
  class Node(val lemma: Lemma, var tag: Tag) {
    private var _parent: Option[Node] = None
    private var _children: Seq[Node] = Seq()

    def addChild(child: Node): Unit = {
      require(child._parent.isEmpty)
      child._parent = Some(this)
      _children :+= child
    }

    def children: Seq[Node] = _children
    def parent: Option[Node] = _parent
    def descendants: Set[Node] = children.toSet ++ children.flatMap(_.descendants)
    def leaves: Set[Node] =
      if(children.isEmpty) {
        Set(this)
      } else {
        children.flatMap(_.leaves).toSet
      }
    def pathToRoot: Seq[Node] = parent match {
      case None => Seq(this)
      case Some(p) => this +: p.pathToRoot
    }
  }

  val root = new Node(rootLemma, rootTag)
  def nodes: Set[Node] = Set(root) ++ root.descendants
  def leaves: Set[Node] = root.leaves
}
