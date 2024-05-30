package nyub.orthogit.git

import nyub.orthogit.storage.{LabelStorage, ObjectStorage}

trait Git[Obj, Id, Label, PathElement]:
    opaque type CommitId = Id

    protected def objectStorage: ObjectStorage[StoredObjects, Id]
    protected def labelStorage: LabelStorage[Label, Id]
    final protected val stagingArea = StagingArea[PathElement, Obj, Id](
      StagingTree.emptyRoot[PathElement, Obj, Id]
    )

    private var headPointer: Option[Id] = None
    private var currentBranch: Option[Label] = None

    sealed trait StoredObjects
    object StoredObjects:
        case class Blob(val obj: Obj) extends StoredObjects
        case class Tree(val childrenIds: Map[PathElement, Id])
            extends StoredObjects

        case class Commit(val parentId: Option[Id], val treeId: Id)
            extends StoredObjects

    def add(obj: StagedObject[PathElement, Obj]) = stagingArea.update(obj)

    def head: Option[CommitId] = headPointer

    def commit(): CommitId =
        val treeId = stagingArea.root.store(
          o => objectStorage.store(StoredObjects.Blob(o)),
          t => objectStorage.store(StoredObjects.Tree(t))
        )
        val commitObject = StoredObjects.Commit(headPointer, treeId)
        val id = objectStorage.store(commitObject)
        updateHead(id)
        updateBranch(id)
        id

    def checkout(id: Id): Unit = objectStorage.get(id) match
        case Some(StoredObjects.Commit(_, _)) =>
            stagingArea.clean()
            currentBranch = None // detached HEAD
            headPointer = Some(id)
        case _ => ???

    def checkoutBranch(label: Label): CommitId =
        labelStorage.get(label) match
            case Some(id) =>
                objectStorage.get(id) match
                    case Some(StoredObjects.Commit(_, _)) =>
                        headPointer = Some(id)
                        currentBranch = Some(label)
                        id
                    case _ => ???
            case None => ???

    def branch(name: Label, id: CommitId): Unit = labelStorage.set(name, id)

    private def updateHead(commitId: CommitId): Unit = headPointer = Some(
      commitId
    )

    private def updateBranch(commitId: CommitId): Unit = currentBranch match
        case Some(label) => labelStorage.set(label, commitId)
        case None        => ()

end Git
