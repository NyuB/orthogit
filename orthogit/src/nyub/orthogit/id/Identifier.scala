package nyub.orthogit.id

trait Identifier[Obj, Id]:
    def id(obj: Obj): Id
