package nyub.orthogit.git

case class StagedObject[PathElement, Obj](
    val path: ObjectPath[PathElement],
    val obj: Obj
)

type GetStableChildren[PathElement, Obj, Id] =
    Id => Map[PathElement, StagingTree.StableTree[PathElement, Obj, Id]]

final class StagingArea[PathElement, Obj, Id](
    var root: StagingTree.StagingNode[PathElement, Obj, Id],
    val getStableChildren: GetStableChildren[PathElement, Obj, Id]
):
    def update(so: StagedObject[PathElement, Obj]): Unit =
        root = root.toNode(getStableChildren).updated(so, getStableChildren)

    def remove(so: ObjectPath[PathElement]) =
        root = root.toNode(getStableChildren).remove(so, getStableChildren)

    def reset(newRoot: StagingTree.StagingNode[PathElement, Obj, Id]): Unit =
        root = newRoot

    def staged: Seq[ObjectPath[PathElement]] = root.stagedLeaves

sealed trait StagingTree[PathElement, Obj, Id]:
    def store(storeObj: Obj => Id, storeTree: Map[PathElement, Id] => Id): Id

object StagingTree:
    def emptyRoot[PathElement, Obj, Id]: Node[PathElement, Obj, Id] =
        Node[PathElement, Obj, Id](Map.empty)

    sealed trait StagingNode[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]:
        def toNode(
            getStableChildren: GetStableChildren[PathElement, Obj, Id]
        ): Node[PathElement, Obj, Id]

        def stagedLeaves: Seq[ObjectPath[PathElement]]

    sealed trait StableTree[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]:
        def id: Id
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id = id

    case class StableLeaf[PathElement, Obj, Id](override val id: Id)
        extends StableTree[PathElement, Obj, Id]

    case class StableNode[PathElement, Obj, Id](override val id: Id)
        extends StableTree[PathElement, Obj, Id]
        with StagingNode[PathElement, Obj, Id]:
        override def toNode(
            getStableChildren: GetStableChildren[PathElement, Obj, Id]
        ): Node[PathElement, Obj, Id] =
            Node(getStableChildren(id))

        override def stagedLeaves: Seq[ObjectPath[PathElement]] = Seq.empty

    sealed trait StagedTree[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]

    case class Leaf[PathElement, Obj, Id](val obj: Obj)
        extends StagedTree[PathElement, Obj, Id]:
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            storeObj(obj)

    case class Node[PathElement, Obj, Id](
        val children: Map[PathElement, StagingTree[PathElement, Obj, Id]]
    ) extends StagedTree[PathElement, Obj, Id]
        with StagingNode[PathElement, Obj, Id]:
        override def toNode(g: GetStableChildren[PathElement, Obj, Id]) = this
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            val tree =
                this.children.view.mapValues(_.store(storeObj, storeTree)).toMap
            storeTree(tree)

        def remove(
            path: ObjectPath[PathElement],
            getStableChildren: GetStableChildren[PathElement, Obj, Id]
        ): Node[PathElement, Obj, Id] =
            if path.path.isEmpty then Node(children.removed(path.name))
            else
                val pathHead = path.path.head
                children.get(pathHead) match
                    case None                => this
                    case Some(Leaf(_))       => this
                    case Some(StableLeaf(_)) => this
                    case Some(t: Node[PathElement, Obj, Id]) =>
                        Node(
                          children.updated(
                            path.head,
                            t.remove(
                              ObjectPath(path.path.tail, path.name),
                              getStableChildren
                            )
                          )
                        )
                    case Some(t: StableNode[PathElement, Obj, Id]) =>
                        Node(
                          children.updated(
                            path.head,
                            t.toNode(getStableChildren)
                                .remove(
                                  ObjectPath(path.path.tail, path.name),
                                  getStableChildren
                                )
                          )
                        )

        def updated(
            obj: StagedObject[PathElement, Obj],
            getStableChildren: GetStableChildren[PathElement, Obj, Id]
        ): Node[PathElement, Obj, Id] =
            if obj.path.path.isEmpty then
                Node(this.children.updated(obj.path.name, Leaf(obj.obj)))
            else
                val pathHead = obj.path.path.head
                val pathTail = obj.path.path.tail
                val next =
                    StagedObject(ObjectPath(pathTail, obj.path.name), obj.obj)
                children.get(pathHead) match
                    case Some(t: Node[PathElement, Obj, Id]) =>
                        Node(
                          this.children.updated(
                            pathHead,
                            t.updated(next, getStableChildren)
                          )
                        )
                    case Some(StableNode(id)) =>
                        val stableChildren = getStableChildren(id)
                        Node(
                          stableChildren.updated(
                            pathHead,
                            StagingTree.emptyRoot
                                .updated(next, getStableChildren)
                          )
                        )
                    case _ =>
                        Node(
                          this.children.updated(
                            pathHead,
                            emptyRoot[PathElement, Obj, Id]
                                .updated(next, getStableChildren)
                          )
                        )

        override def stagedLeaves: Seq[ObjectPath[PathElement]] =
            this.children
                .flatMap: (path, t) =>
                    t match
                        case _: StableTree[PathElement, Obj, Id] => Seq.empty
                        case t: StagedTree[PathElement, Obj, Id] =>
                            leaves(ObjectPath(Seq.empty, path), t)
                .toSeq

        private def leaves(
            prev: ObjectPath[PathElement],
            node: StagedTree[PathElement, Obj, Id]
        ): Seq[ObjectPath[PathElement]] = node match
            case Node(c) =>
                c.flatMap: (path, t) =>
                    t match
                        case _: StableTree[PathElement, Obj, Id] => Seq.empty
                        case t: StagedTree[PathElement, Obj, Id] =>
                            leaves(ObjectPath(prev.path :+ prev.name, path), t)
                .toSeq
            case Leaf(_) => Seq(prev)
