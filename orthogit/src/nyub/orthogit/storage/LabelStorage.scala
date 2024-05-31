package nyub.orthogit.storage

trait LabelStorage[Label, Obj]:
    def delete(label: Label): Option[Obj]
    def set(label: Label, obj: Obj): Unit
    def get(label: Label): Option[Obj]

object LabelStorage:
    class InMemory[Label, Obj](
        private val map: scala.collection.mutable.Map[Label, Obj]
    ) extends LabelStorage[Label, Obj]:
        override def delete(label: Label): Option[Obj] = map.remove(label)
        override def set(label: Label, obj: Obj): Unit = map.update(label, obj)
        override def get(label: Label): Option[Obj] = map.get(label)
