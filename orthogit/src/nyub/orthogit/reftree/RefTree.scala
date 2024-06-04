package nyub.orthogit.reftree

sealed trait RefTree[+NodeId, +LeafId, +Edge, +Leaf] derives CanEqual
case class RefNode[+NodeId](val id: NodeId)
    extends RefTree[NodeId, Nothing, Nothing, Nothing] derives CanEqual

case class RefLeaf[+LeafId](val id: LeafId)
    extends RefTree[Nothing, LeafId, Nothing, Nothing] derives CanEqual

case class ValueNode[+NodeId, +LeafId, Edge, +Leaf](
    children: Map[Edge, RefTree[NodeId, LeafId, Edge, Leaf]]
) extends RefTree[NodeId, LeafId, Edge, Leaf]
    derives CanEqual

case class ValueLeaf[+Leaf](value: Leaf)
    extends RefTree[Nothing, Nothing, Nothing, Leaf] derives CanEqual
