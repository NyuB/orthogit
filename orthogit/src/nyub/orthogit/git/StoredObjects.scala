package nyub.orthogit.git

sealed trait StoredObjects[+Obj, +Id, +PathElement, +Meta]
object StoredObjects:
    case class Blob[+Obj](val obj: Obj)
        extends StoredObjects[Obj, Nothing, Nothing, Nothing]

    case class Tree[+Id, PathElement](
        val childrenIds: Map[PathElement, Id]
    ) extends StoredObjects[Nothing, Id, PathElement, Nothing]

    case class Commit[+Id, +Meta](
        val parentIds: Seq[Id],
        val treeId: Id,
        val metadata: Meta
    ) extends StoredObjects[Nothing, Id, Nothing, Meta]
