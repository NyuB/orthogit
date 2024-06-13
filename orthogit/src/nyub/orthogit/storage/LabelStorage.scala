package nyub.orthogit.storage

/** Mutable key/value store
  */
trait LabelStorage[Label, Obj]:
    def delete(label: Label): Option[Obj]
    def set(label: Label, obj: Obj): Unit
    def get(label: Label): Option[Obj]
    def map[B](
        mapping: Obj => Option[B],
        reverse: B => Option[Obj]
    ): LabelStorage[Label, B] = LabelStorage.Delegated(this, mapping, reverse)

object LabelStorage:
    class InMemory[Label, Obj](
        private val map: scala.collection.mutable.Map[Label, Obj]
    ) extends LabelStorage[Label, Obj]:
        def this() = this(scala.collection.mutable.Map.empty)
        override def delete(label: Label): Option[Obj] = map.remove(label)
        override def set(label: Label, obj: Obj): Unit = map.update(label, obj)
        override def get(label: Label): Option[Obj] = map.get(label)

    private class Delegated[Label, A, B](
        private val delegate: LabelStorage[Label, A],
        private val mapping: A => Option[B],
        private val reverse: B => Option[A]
    ) extends LabelStorage[Label, B]:
        override def delete(label: Label): Option[B] =
            delegate.delete(label).flatMap(mapping)

        override def get(label: Label): Option[B] =
            delegate.get(label).flatMap(mapping)

        override def set(label: Label, obj: B): Unit = reverse(obj) match
            case None    => ()
            case Some(a) => delegate.set(label, a)
