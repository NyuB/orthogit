package nyub.orthogit.git

case class StagedObject[PathElement, Obj](
    val path: ObjectPath[PathElement],
    val obj: Obj
)
