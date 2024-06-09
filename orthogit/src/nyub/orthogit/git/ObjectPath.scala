package nyub.orthogit.git

case class ObjectPath[+PathElement](
    val path: Seq[PathElement],
    val name: PathElement
)(using CanEqual[PathElement, PathElement])
    derives CanEqual:
    def isLeaf = path.isEmpty
    def head = path.head
    def tail: ObjectPath[PathElement] = ObjectPath(path.tail, name)
    def resolve[P >: PathElement](using CanEqual[P, P])(
        pathElement: P
    ): ObjectPath[P] = ObjectPath(path :+ name, pathElement)

    infix def /[P >: PathElement](using CanEqual[P, P])(
        pathElement: P
    ): ObjectPath[P] = resolve(pathElement)

    def contains[P >: PathElement](using
        CanEqual[P, PathElement]
    )(other: ObjectPath[P]): Boolean =
        if other.path.size > this.path.size then false
        else if other.path.size == this.path.size then
            other.name == this.name && other.path == this.path
        else
            scala.util.boundary:
                var i = 0
                while i < other.path.size do
                    if other.path(i) != this.path(i) then
                        scala.util.boundary.break(false)
                    i += 1
                other.name == this.path(i)

object ObjectPath:
    def apply[PathElement](name: PathElement)(using
        CanEqual[PathElement, PathElement]
    ) = new ObjectPath(Seq.empty, name)
