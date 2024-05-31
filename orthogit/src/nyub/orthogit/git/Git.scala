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

    final private lazy val stagingArea = initStagingArea

    private def initStagingArea: StagingArea[PathElement, Obj, Id] =
        val root = headTreeId
            .map(StagingTree.StableNode[PathElement, Obj, Id](_))
            .getOrElse(StagingTree.emptyRoot[PathElement, Obj, Id])
        StagingArea(root, getStagingChildren)

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
        stagingArea.reset(StagingTree.StableNode(treeId))
        id

    def checkout(id: CommitId): Unit = objectStorage.get(id) match
        case Some(StoredObjects.Commit(_, treeId)) =>
            stagingArea.reset(StagingTree.StableNode(treeId))
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

    private def headTreeId: Option[Id] =
        head.get
            .flatMap(objectStorage.get)
            .collect:
                case c: StoredObjects.Commit[Obj, Id, PathElement] => c.treeId

    private def getHeadTree: StoredObjects.Tree[Obj, Id, PathElement] =
        headTreeId
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

    private def getStagingChildren(
        id: Id
    ): Map[PathElement, StagingTree.StableTree[PathElement, Obj, Id]] =
        objectStorage
            .get(id)
            .collect:
                case StoredObjects.Tree(c) =>
                    c.view
                        .mapValues: cid =>
                            objectStorage
                                .get(cid)
                                .collect:
                                    case StoredObjects.Tree(_) =>
                                        StagingTree
                                            .StableNode[PathElement, Obj, Id](
                                              cid
                                            )
                                    case StoredObjects.Blob(_) =>
                                        StagingTree
                                            .StableLeaf[PathElement, Obj, Id](
                                              cid
                                            )
                                .getOrElse(?!!)
                        .toMap
            .getOrElse(?!!)

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
