package nyub.orthogit.git

import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit

trait Git[Obj, Id, Label, PathElement, Meta]:
    opaque type CommitId = Id

    protected def objectStorage
        : ObjectStorage[StoredObjects[Obj, Id, PathElement, Meta], Id]

    protected def labelStorage: LabelStorage[Label, Id]

    protected def head: Head[Id]

    protected def currentBranch: Head[Label]

    /** @todo extract agnostic interface and delegate to trait implementers */
    final private lazy val stagingArea
        : InMemoryStagingArea[PathElement, Obj, Id] = initStagingArea

    private def initStagingArea: InMemoryStagingArea[PathElement, Obj, Id] =
        val root = headTreeId
            .map(StagingTree.stableTree[PathElement, Obj, Id])
            .getOrElse(StagingTree.emptyRoot[PathElement, Obj, Id])
        InMemoryStagingArea(root, unsafeGetStagingChildren)

    /** Stages `obj`, adding it to the bulk of objects that would be part of a
      * [[Git.commit]]. If there is already a staged object at this path, it
      * will be replaced. If there is no staged object at this path, it will be
      * created.
      * @param obj
      *   a path and the data to update at this path
      */
    def add(stagedObject: StagedObject[PathElement, Obj]): Unit =
        stagingArea.update(stagedObject)

    /** Creates a new commit from the updates of the staging area.
      *
      * Additional side effects:
      *   - Clears the staging area
      *   - Updates the current branch to point to the created commit
      *   - Updates the current
      * head to point to the created commit
      * @param meta
      *   the metadata to associate with this commit
      * @return
      *   the id of the created commit
      * @see
      *   [[Git.add]] to stage objects before committing
      */
    def commit(meta: Meta): CommitId =
        val treeId = stagingArea.synchronize(
          o => objectStorage.store(StoredObjects.Blob(o)),
          t => objectStorage.store(StoredObjects.Tree(t))
        )
        val commitObject =
            StoredObjects.Commit[Obj, Id, PathElement, Meta](
              head.get,
              treeId,
              meta
            )
        val commitId = objectStorage.store(commitObject)
        head.set(Some(commitId))
        updateBranch(commitId)
        stagingArea.reset(StagingTree.stableTree(treeId))
        commitId

    /** @param commitId
      *   a commit id, e.g. retrieved from [[Git.commit]]
      * @return
      *   the commit associated with this id
      */
    def show(
        commitId: CommitId
    ): StoredObjects.Commit[Obj, Id, PathElement, Meta] = // TODO define a proper api for such 'status' operations to avoid exposing low-level StoredObjects
        objectStorage.get(commitId) match
            case Some(c: StoredObjects.Commit[Obj, Id, PathElement, Meta]) => c
            case _ => ?!!

    /** Positions the current head at the commit indicated by `commitId`
      *
      * Additional side effects:
      *   - Clears the staging area
      *   - Unsets the current branch if any ('detached HEAD')
      * @param commitId
      *   id of the commit to move to
      */
    def checkout(commitId: CommitId): Unit = objectStorage.get(commitId) match
        case Some(StoredObjects.Commit(_, treeId, _)) =>
            stagingArea.reset(StagingTree.stableTree(treeId))
            currentBranch.unset() // detached HEAD
            head.set(Some(commitId))
        case _ => ???

    /** [[Git.checkout]] to the commit indicated by `label` and positions the
      * current branch to `label`
      *
      * @param label
      *   label of the branch to go to
      * @return
      *   the head commit id after checkout
      */
    def checkoutLabel(label: Label): CommitId =
        labelStorage.get(label) match
            case Some(id) =>
                objectStorage.get(id) match
                    case Some(StoredObjects.Commit(_, _, _)) =>
                        checkout(id)
                        currentBranch.set(Some(label))
                        id
                    case _ => ?!!
            case None => ???

    /** Makes the branch with name `label` point to commit `commitId`.
      *
      * @param name
      *   label of the branch. If it did not exist it is created, if it existed
      *   it is updated.
      * @param commitId
      *   commit id to point to
      */
    def branch(name: Label, commitId: CommitId): Unit =
        labelStorage.set(name, commitId)

    /** Retrieves the object stored at path `objectPath` in the current head's
      * tree, if any.
      *
      * @param objectPath
      *   the path to the desired object
      * @return
      *   `Some(obj)` if `obj` is indeed present ath this path in the current
      *   tree, or `None` if:
      *   - There is no object at this path in the current tree
      *   - The current head is not pointing to any commit
      */
    def get(objectPath: ObjectPath[PathElement]): Option[Obj] =
        get(getHeadTree, objectPath)

    /** @return
      *   all paths leading to objects staged for update
      */
    def staged: Seq[ObjectPath[PathElement]] = stagingArea.staged

    private def get(
        tree: StoredObjects.Tree[Obj, Id, PathElement, Meta],
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
                    case t: StoredObjects.Tree[Obj, Id, PathElement, Meta] => t
                .flatMap(t => get(t, objectPath.tail))

    private def headTreeId: Option[Id] =
        head.get
            .flatMap(objectStorage.get)
            .collect:
                case c: StoredObjects.Commit[Obj, Id, PathElement, Meta] =>
                    c.treeId

    private def getHeadTree: StoredObjects.Tree[Obj, Id, PathElement, Meta] =
        headTreeId
            .flatMap(objectStorage.get)
            .collect:
                case t: StoredObjects.Tree[_, _, _, _] => t
            .getOrElse(?!!)

    private def updateBranch(commitId: CommitId): Unit = currentBranch.get match
        case Some(label) => labelStorage.set(label, commitId)
        case None        => ()

    private def ?!! = throw IllegalStateException(
      "Panic, reached illegal state"
    )

    private def unsafeGetStagingChildren(
        id: Id
    ): Map[PathElement, StagingTree.StableTree[PathElement, Obj, Id]] =
        val tree = unsafeGetTree(id)
        tree.childrenIds.view
            .mapValues: cid =>
                objectStorage
                    .get(cid)
                    .collect:
                        case StoredObjects.Tree(_) =>
                            StagingTree
                                .stableTree[PathElement, Obj, Id](
                                  cid
                                )
                        case StoredObjects.Blob(_) =>
                            StagingTree
                                .stableObject[PathElement, Obj, Id](
                                  cid
                                )
                    .getOrElse(?!!)
            .toMap

    private def unsafeGetTree(
        treeId: Id
    ): StoredObjects.Tree[Obj, Id, PathElement, Meta] =
        objectStorage
            .get(treeId)
            .collect:
                case t: StoredObjects.Tree[Obj, Id, PathElement, Meta] => t
            .getOrElse(?!!)

end Git

sealed trait StoredObjects[Obj, Id, PathElement, Meta]
object StoredObjects:
    case class Blob[Obj, Id, PathElement, Meta](val obj: Obj)
        extends StoredObjects[Obj, Id, PathElement, Meta]

    case class Tree[Obj, Id, PathElement, Meta](
        val childrenIds: Map[PathElement, Id]
    ) extends StoredObjects[Obj, Id, PathElement, Meta]

    case class Commit[Obj, Id, PathElement, Meta](
        val parentId: Option[Id],
        val treeId: Id,
        val metadata: Meta
    ) extends StoredObjects[Obj, Id, PathElement, Meta]

trait Head[Id]:
    def get: Option[Id]
    def set(id: Option[Id]): Unit
    final def unset(): Unit = set(None)
