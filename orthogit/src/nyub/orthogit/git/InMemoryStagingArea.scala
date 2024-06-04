package nyub.orthogit.git

object InMemoryStagingArea:
    type GetStableChildren[PathElement, Obj, Id] =
        Id => Map[PathElement, StagingTree.StableTree[PathElement, Obj, Id]]

/** TODO decouple [[Git]] from this, extract interface agnostic of
  * [[StagingTree]]
  */
final private class InMemoryStagingArea[PathElement, Obj, Id](
    private var root: StagingTree.StagingNode[PathElement, Obj, Id],
    val getStableChildren: InMemoryStagingArea.GetStableChildren[
      PathElement,
      Obj,
      Id
    ]
):
    def update(so: StagedObject[PathElement, Obj]): Unit =
        root = root.toNode(getStableChildren).updated(so, getStableChildren)

    def reset(newRoot: StagingTree.StagingNode[PathElement, Obj, Id]): Unit =
        root = newRoot

    def staged: Seq[ObjectPath[PathElement]] = root.stagedLeaves

    def synchronize(
        storeObj: Obj => Id,
        storeTree: Map[PathElement, Id] => Id
    ): Id =
        root.store(storeObj, storeTree)

sealed private trait StagingTree[PathElement, Obj, Id]:
    def store(storeObj: Obj => Id, storeTree: Map[PathElement, Id] => Id): Id

private object StagingTree:
    def emptyRoot[PathElement, Obj, Id]: Node[PathElement, Obj, Id] =
        Node[PathElement, Obj, Id](Map.empty)

    def stableTree[PathElement, Obj, Id](
        id: Id
    ): StableNode[PathElement, Obj, Id] = StableNode(id)

    def stableObject[PathElement, Obj, Id](
        id: Id
    ): StableLeaf[PathElement, Obj, Id] = StableLeaf(id)

    sealed trait StagingNode[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]:
        def toNode(
            getStableChildren: InMemoryStagingArea.GetStableChildren[
              PathElement,
              Obj,
              Id
            ]
        ): Node[PathElement, Obj, Id]

        def stagedLeaves: Seq[ObjectPath[PathElement]]

    sealed trait StableTree[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]:
        def id: Id
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id = id

    case class StableLeaf[PathElement, Obj, Id] private[StagingTree] (
        override val id: Id
    ) extends StableTree[PathElement, Obj, Id]

    case class StableNode[PathElement, Obj, Id] private[StagingTree] (
        override val id: Id
    ) extends StableTree[PathElement, Obj, Id]
        with StagingNode[PathElement, Obj, Id]:
        override def toNode(
            getStableChildren: InMemoryStagingArea.GetStableChildren[
              PathElement,
              Obj,
              Id
            ]
        ): Node[PathElement, Obj, Id] =
            Node(getStableChildren(id))

        override def stagedLeaves: Seq[ObjectPath[PathElement]] = Seq.empty

    sealed trait StagedTree[PathElement, Obj, Id]
        extends StagingTree[PathElement, Obj, Id]

    case class Leaf[PathElement, Obj, Id] private[StagingTree] (val obj: Obj)
        extends StagedTree[PathElement, Obj, Id]:
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            storeObj(obj)

    case class Node[PathElement, Obj, Id] private[StagingTree] (
        val children: Map[PathElement, StagingTree[PathElement, Obj, Id]]
    ) extends StagedTree[PathElement, Obj, Id]
        with StagingNode[PathElement, Obj, Id]:
        override def toNode(
            g: InMemoryStagingArea.GetStableChildren[PathElement, Obj, Id]
        ) = this

        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            val tree =
                this.children.view.mapValues(_.store(storeObj, storeTree)).toMap
            storeTree(tree)

        def updated(
            obj: StagedObject[PathElement, Obj],
            getStableChildren: InMemoryStagingArea.GetStableChildren[
              PathElement,
              Obj,
              Id
            ]
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

end StagingTree
