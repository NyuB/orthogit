package nyub.orthogit.storage

import nyub.orthogit.id.Identifier

/** Mutable Key/value store where key generation is handled by the storage.
  * Should generate the same id for two equal objects.
  */
trait ObjectStorage[Obj, Id]:
    def store(obj: Obj): Id
    def get(id: Id): Option[Obj]

object ObjectStorage:
    class InMemory[Obj, Id](using identifier: Identifier[Obj, Id])(
        private val map: scala.collection.mutable.Map[Id, Obj]
    ) extends ObjectStorage[Obj, Id]:
        override def get(id: Id): Option[Obj] = map.get(id)
        override def store(obj: Obj): Id =
            val id = identifier.id(obj)
            map.update(id, obj)
            id

    object InMemory:
        def apply[Obj, Id](using Identifier[Obj, Id])() = new InMemory(
          scala.collection.mutable.Map.empty
        )
