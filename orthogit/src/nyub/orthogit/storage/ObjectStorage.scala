package nyub.orthogit.storage

trait ObjectStorage[Obj, Id]:
    def store(obj: Obj): Id
    def get(id: Id): Option[Obj]
