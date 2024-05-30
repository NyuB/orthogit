package nyub.orthogit.git

case class StagedObject[PathElement, Obj](
    val path: ObjectPath[PathElement],
    val obj: Obj
)

final class StagingArea[PathElement, Obj, Id](
    var root: StagingTree.Node[PathElement, Obj, Id]
):
    def update(so: StagedObject[PathElement, Obj]): Unit =
        root = root.updated(so)

    def remove(so: ObjectPath[PathElement]) =
        root = root.remove(so)

    def clean(): Unit =
        root = StagingTree.Node(Map.empty)

sealed trait StagingTree[PathElement, Obj, Id]:
    def store(storeObj: Obj => Id, storeTree: Map[PathElement, Id] => Id): Id

object StagingTree:
    def emptyRoot[PathElement, Obj, Id]: Node[PathElement, Obj, Id] =
        Node[PathElement, Obj, Id](Map.empty)

    case class Leaf[PathElement, Obj, Id](val obj: Obj)
        extends StagingTree[PathElement, Obj, Id]:
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            storeObj(obj)

    case class Node[PathElement, Obj, Id](
        val children: Map[PathElement, StagingTree[PathElement, Obj, Id]]
    ) extends StagingTree[PathElement, Obj, Id]:
        override def store(
            storeObj: Obj => Id,
            storeTree: Map[PathElement, Id] => Id
        ): Id =
            val tree =
                this.children.view.mapValues(_.store(storeObj, storeTree)).toMap
            storeTree(tree)

        def remove(path: ObjectPath[PathElement]): Node[PathElement, Obj, Id] =
            if path.path.isEmpty then Node(children.removed(path.name))
            else
                val pathHead = path.path.head
                children.get(pathHead) match
                    case None          => this
                    case Some(Leaf(_)) => this
                    case Some(t: Node[PathElement, Obj, Id]) =>
                        Node(
                          children.updated(
                            path.path.head,
                            t.remove(ObjectPath(path.path.tail, path.name))
                          )
                        )

        def updated(
            obj: StagedObject[PathElement, Obj]
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
                        Node(this.children.updated(pathHead, t.updated(next)))
                    case _ =>
                        Node(
                          this.children.updated(
                            pathHead,
                            emptyRoot[PathElement, Obj, Id].updated(next)
                          )
                        )
