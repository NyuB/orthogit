package nyub.orthogit.git

import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit

trait Git[Obj, Id, Label, PathElement, Meta]:
    opaque type BlobId = Id
    opaque type CommitId = Id
    opaque type TreeId = Id

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
      *   - Updates the current head to point to the created commit
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

    /** Stores a new commit object
      * @param commit
      *   the commit object to create
      * @return
      *   the id of the created commit
      * @see
      *   Git.writeTree
      * @see
      *   Git.writeBlob
      */
    def writeCommit(commit: this.Commit): CommitId =
        val commitObject = StoredObjects.Commit[Obj, Id, PathElement, Meta](
          commit.parentId,
          commit.treeId,
          commit.meta
        )
        objectStorage.store(commitObject)

    /** @param commitId
      *   a commit id, e.g. retrieved from [[Git.commit]] or [[Git.writeCommit]]
      * @return
      *   the commit associated with this id
      */
    def getCommit(
        commitId: CommitId
    ): this.Commit =
        objectStorage.get(commitId) match
            case Some(StoredObjects.Commit(parentId, treeId, meta)) =>
                Commit(parentId, treeId, meta)
            case _ => ?!!

    /** Stores a new tree object
      *
      * Children that are not already stored [[TreeRef]] or [[BlobRef]] are
      * stored recursively before the root
      *
      * @param tree
      *   the object structure to store
      * @return
      *   the id of the stored tree
      */
    def writeTree(tree: this.Tree): TreeId =
        val storedChildren = tree.children.map: (path, ref) =>
            ref match
                case TreeRef(treeId) => path -> treeId
                case BlobRef(blobId) => path -> blobId
                case t: Tree         => path -> writeTree(t)
                case b: Blob         => path -> writeBlob(b)
        val storedTree =
            StoredObjects.Tree[Obj, Id, PathElement, Meta](storedChildren)
        objectStorage.store(storedTree)

    /** @param treeId
      *   a tree id, e.g. retrieved from a [[Commit]] or another [[Tree]]
      * @return
      *   the tree associated with this id
      */
    def getTree(treeId: TreeId): this.Tree =
        objectStorage
            .get(treeId)
            .collect:
                case StoredObjects.Tree[Obj, Id, PathElement, Meta](children) =>
                    val mapped = children.view
                        .mapValues(id => id -> objectStorage.get(id))
                        .mapValues:
                            case id -> Some(StoredObjects.Blob(_)) =>
                                BlobRef(id)
                            case id -> Some(StoredObjects.Tree(_)) =>
                                TreeRef(id)
                            case _ => ?!!
                    Tree(mapped.toMap)
            .getOrElse(?!!)

    /** Stores a new blob object
      *
      * @param blob
      *   the blob to store
      * @return
      *   the id of the stored blob
      */
    def writeBlob(blob: Blob): BlobId =
        objectStorage.store(
          StoredObjects.Blob[Obj, Id, PathElement, Meta](blob.obj)
        )

    /** @param blobId
      * @return
      */
    def getBlob(blobId: BlobId): this.Blob =
        objectStorage
            .get(blobId)
            .collect:
                case StoredObjects.Blob[Obj, Id, PathElement, Meta](obj) =>
                    Blob(obj)
            .getOrElse(?!!)

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
      *   `Some(obj)` if `obj` is indeed present at this path in the current
      *   tree, or `None` if:
      *   - There is no object at this path in the current tree
      *   - The current head is not pointing to any commit
      */
    def getObject(objectPath: ObjectPath[PathElement]): Option[Obj] =
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

    case class Commit(
        val parentId: Option[CommitId],
        val treeId: TreeId,
        val meta: Meta
    ) derives CanEqual

    sealed trait TreeItem
    case class TreeRef(val treeId: TreeId) extends TreeItem derives CanEqual
    case class Tree(val children: Map[PathElement, TreeItem]) extends TreeItem
        derives CanEqual

    case class Blob(val obj: Obj) extends TreeItem derives CanEqual
    case class BlobRef(val blobId: BlobId) extends TreeItem derives CanEqual

end Git

trait Head[Id]:
    def get: Option[Id]
    def set(id: Option[Id]): Unit
    final def unset(): Unit = set(None)
