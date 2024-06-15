package nyub.orthogit.reftree

sealed trait RefTree[+NodeId, +LeafId, +Edge, +Leaf] derives CanEqual
sealed trait Node[+NodeId, +LeafId, +Edge, +Leaf]
    extends RefTree[NodeId, LeafId, Edge, Leaf] derives CanEqual

sealed trait Ref[+NodeId, +LeafId]
    extends RefTree[NodeId, LeafId, Nothing, Nothing] derives CanEqual:
    def id: NodeId | LeafId

extension [NodeId, LeafId, Edge, Leaf](t: Node[NodeId, LeafId, Edge, Leaf])
    def compress(
        compressLeaf: Leaf => LeafId,
        compressNode: Map[Edge, Ref[NodeId, LeafId]] => NodeId
    ): NodeId =
        t match
            case RefNode(id) => id
            case ValueNode(children) =>
                val compressedChildren = children.view.mapValues:
                    case RefNode(id) => RefNode(id)
                    case RefLeaf(id) => RefLeaf(id)
                    case ValueNode(children) =>
                        RefNode(
                          ValueNode(children.toMap).compress(
                            compressLeaf,
                            compressNode
                          )
                        )
                    case ValueLeaf(value) => RefLeaf(compressLeaf(value))
                compressNode(compressedChildren.toMap)

extension [NodeId, LeafId, Edge, Leaf](node: Node[NodeId, LeafId, Edge, Leaf])
    def expand(
        expandLeaf: LeafId => Leaf,
        expandNode: NodeId => Map[Edge, RefTree[NodeId, LeafId, Edge, Leaf]]
    ): ValueNode[NodeId, LeafId, Edge, Leaf] =
        node match
            case RefNode(id) =>
                ValueNode(expandNode(id)).expand(expandLeaf, expandNode)
            case ValueNode(children) =>
                val expandedChildren = children.view.mapValues:
                    case n: Node[NodeId, LeafId, Edge, Leaf] =>
                        n.expand(expandLeaf, expandNode)
                    case l: RefLeaf[LeafId] => l.expand(expandLeaf)
                    case l: ValueLeaf[Leaf] => l
                ValueNode(expandedChildren.toMap)

    def insert(
        at: Edge,
        path: Seq[Edge],
        tree: RefTree[NodeId, LeafId, Edge, Leaf],
        expandNode: NodeId => Map[Edge, Ref[NodeId, LeafId]]
    ): Node[NodeId, LeafId, Edge, Leaf] =
        node match
            case RefNode(id) =>
                ValueNode(expandNode(id)).insert(at, path, tree, expandNode)
            case ValueNode(children) =>
                if path.isEmpty then ValueNode(children.toMap.updated(at, tree))
                else
                    val updated = children.toMap.updatedWith(path.head):
                        case Some(n: Node[NodeId, LeafId, Edge, Leaf]) =>
                            Some(n.insert(at, path.tail, tree, expandNode))
                        case _ =>
                            Some(
                              ValueNode(Map.empty)
                                  .insert(at, path.tail, tree, expandNode)
                            )
                    ValueNode(updated)

case class RefNode[+NodeId](override val id: NodeId)
    extends Node[NodeId, Nothing, Nothing, Nothing],
      Ref[NodeId, Nothing] derives CanEqual

case class RefLeaf[+LeafId](override val id: LeafId)
    extends Ref[Nothing, LeafId] derives CanEqual:
    def expand[T](ex: LeafId => T): ValueLeaf[T] = ValueLeaf(ex(id))

case class ValueNode[+NodeId, +LeafId, Edge, +Leaf](
    children: Map[Edge, RefTree[NodeId, LeafId, Edge, Leaf]]
) extends Node[NodeId, LeafId, Edge, Leaf]
    derives CanEqual

case class ValueLeaf[+Leaf](value: Leaf)
    extends RefTree[Nothing, Nothing, Nothing, Leaf] derives CanEqual

object RefTree:
    def empty: Node[Nothing, Nothing, Nothing, Nothing] = ValueNode(
      Map.empty
    )
