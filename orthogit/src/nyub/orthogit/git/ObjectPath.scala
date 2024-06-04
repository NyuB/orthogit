package nyub.orthogit.git

case class ObjectPath[+PathElement](
    val path: Seq[PathElement],
    val name: PathElement
) derives CanEqual:
    def isLeaf = path.isEmpty
    def head = path.head
    def tail: ObjectPath[PathElement] = ObjectPath(path.tail, name)
