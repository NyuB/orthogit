package nyub.orthogit.git

case class StagedObject[+PathElement, +Obj](
    val path: ObjectPath[PathElement],
    val obj: Obj
):
    def map[T](f: Obj => T): StagedObject[PathElement, T] =
        StagedObject(path, f(obj))
