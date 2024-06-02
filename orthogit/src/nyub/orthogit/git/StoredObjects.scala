package nyub.orthogit.git

sealed trait StoredObjects[Obj, Id, PathElement, Meta]
object StoredObjects:
    case class Blob[Obj, Id, PathElement, Meta](val obj: Obj)
        extends StoredObjects[Obj, Id, PathElement, Meta]

    case class Tree[Obj, Id, PathElement, Meta](
        val childrenIds: Map[PathElement, Id]
    ) extends StoredObjects[Obj, Id, PathElement, Meta]

    case class Commit[Obj, Id, PathElement, Meta](
        val parentId: Option[Id],
        val treeId: Id,
        val metadata: Meta
    ) extends StoredObjects[Obj, Id, PathElement, Meta]
