package nyub.orthogit.git

import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit

trait Git[Obj, Id, Label, PathElement]:
    opaque type CommitId = Id

    protected def objectStorage
        : ObjectStorage[StoredObjects[Obj, Id, PathElement], Id]

    protected def labelStorage: LabelStorage[Label, Id]

    protected def head: Head[Id]

    protected def currentBranch: Head[Label]

    final protected val stagingArea = StagingArea[PathElement, Obj, Id](
      StagingTree.emptyRoot[PathElement, Obj, Id]
    )

    def add(obj: StagedObject[PathElement, Obj]) = stagingArea.update(obj)

    def commit(): CommitId =
        val treeId = stagingArea.root.store(
          o => objectStorage.store(StoredObjects.Blob(o)),
          t => objectStorage.store(StoredObjects.Tree(t))
        )
        val commitObject =
            StoredObjects.Commit[Obj, Id, PathElement](head.get, treeId)
        val id = objectStorage.store(commitObject)
        head.set(Some(id))
        updateBranch(id)
        id

    def checkout(id: CommitId): Unit = objectStorage.get(id) match
        case Some(StoredObjects.Commit(_, _)) =>
            stagingArea.clean()
            currentBranch.unset() // detached HEAD
            head.set(Some(id))
        case _ => ???

    def checkoutBranch(label: Label): CommitId =
        labelStorage.get(label) match
            case Some(id) =>
                objectStorage.get(id) match
                    case Some(StoredObjects.Commit(_, _)) =>
                        head.set(Some(id))
                        currentBranch.set(Some(label))
                        id
                    case _ => ???
            case None => ???

    def branch(name: Label, id: CommitId): Unit = labelStorage.set(name, id)

    def get(objectPath: ObjectPath[PathElement]): Option[Obj] =
        get(getHeadTree, objectPath)

    def staged: Seq[ObjectPath[PathElement]] = stagingArea.staged

    private def get(
        tree: StoredObjects.Tree[Obj, Id, PathElement],
        objectPath: ObjectPath[PathElement]
    ): Option[Obj] =
        if objectPath.isLeaf then
            tree.childrenIds
                .get(objectPath.name)
                .flatMap(objectStorage.get)
                .collect:
                    case StoredObjects.Blob(obj) => obj
        else
            tree.childrenIds
                .get(objectPath.head)
                .flatMap(objectStorage.get)
                .collect:
                    case t: StoredObjects.Tree[Obj, Id, PathElement] => t
                .flatMap(t => get(t, objectPath.tail))

    private def getHeadTree: StoredObjects.Tree[Obj, Id, PathElement] =
        objectStorage
            .get(head.get.get)
            .collect:
                case c: StoredObjects.Commit[Obj, Id, PathElement] => c.treeId
            .flatMap(objectStorage.get)
            .collect:
                case t: StoredObjects.Tree[_, _, _] => t
            .getOrElse(?!!)

    private def updateBranch(commitId: CommitId): Unit = currentBranch.get match
        case Some(label) => labelStorage.set(label, commitId)
        case None        => ()

    private def ?!! = throw IllegalStateException(
      "Panic, reached illegal state"
    )

end Git

sealed trait StoredObjects[Obj, Id, PathElement]
object StoredObjects:
    case class Blob[Obj, Id, PathElement](val obj: Obj)
        extends StoredObjects[Obj, Id, PathElement]

    case class Tree[Obj, Id, PathElement](val childrenIds: Map[PathElement, Id])
        extends StoredObjects[Obj, Id, PathElement]

    case class Commit[Obj, Id, PathElement](
        val parentId: Option[Id],
        val treeId: Id
    ) extends StoredObjects[Obj, Id, PathElement]

trait Head[Id]:
    def get: Option[Id]
    def set(id: Option[Id]): Unit
    final def unset(): Unit = set(None)
