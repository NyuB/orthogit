package nyub.orthogit.git

sealed trait StoredObjects[+Obj, +Id, PathElement, +Meta]
object StoredObjects:
    case class Blob[+Obj, PathElement](val obj: Obj)
        extends StoredObjects[Obj, Nothing, PathElement, Nothing]

    case class Tree[+Id, PathElement](
        val childrenIds: Map[PathElement, Id]
    ) extends StoredObjects[Nothing, Id, PathElement, Nothing]

    case class Commit[+Id, PathElement, Meta](
        val parentIds: Seq[Id],
        val treeId: Id,
        val metadata: Meta
    ) extends StoredObjects[Nothing, Id, PathElement, Meta]
