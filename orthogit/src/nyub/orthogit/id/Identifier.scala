package nyub.orthogit.id

trait Identifier[Obj, Id]:
    def id(obj: Obj): Id
    def merge(ids: Seq[Id]): Id
