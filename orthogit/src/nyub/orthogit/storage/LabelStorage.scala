package nyub.orthogit.storage

trait LabelStorage[Label, Obj]:
    def delete(label: Label): Option[Obj]
    def set(label: Label, obj: Obj): Unit
    def get(label: Label): Option[Obj]
