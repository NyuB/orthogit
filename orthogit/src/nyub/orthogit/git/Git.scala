package nyub.orthogit.git

import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit
import nyub.orthogit.reftree.{RefLeaf, RefNode, RefTree, ValueLeaf, ValueNode}
import nyub.orthogit.reftree.{compress, insert}
import nyub.orthogit.reftree.Ref

trait Git[Obj, Id, Label, PathElement, Meta](using
    CanEqual[PathElement, PathElement]
):
    opaque type BlobId = Id
    opaque type CommitId = Id
    opaque type TreeId = Id

    given CanEqual[BlobId, BlobId] = CanEqual.derived
    given CanEqual[CommitId, CommitId] = CanEqual.derived
    given CanEqual[TreeId, TreeId] = CanEqual.derived

    protected def objectStorage
        : ObjectStorage[StoredObjects[Obj, Id, PathElement, Meta], Id]

    protected def labelStorage: LabelStorage[Label, Id]

    protected def head: Head[Id]

    protected def currentBranch: Head[Label]

    protected def stagingArea: StagingArea[TreeId, BlobId, PathElement, Obj]

    /** Stages `obj`, adding it to the bulk of objects that would be part of a
      * [[Git.commit]]. If there is already a staged object at this path, it
      * will be replaced. If there is no staged object at this path, it will be
      * created.
      * @param obj
      *   a path and the data to update at this path
      */
    def add(stagedObject: StagedObject[PathElement, Obj]): Unit =
        stagingArea.add(stagedObject.map(ValueLeaf(_)))

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
        val currentTree = headTreeId.map(TreeRef(_)).getOrElse(Tree(Map.empty))
        val updated = stagingArea.staged.foldLeft(currentTree): (t, s) =>
            t.insert(s.path.name, s.path.path, s.obj, getTreeChildren)
        val treeId = updated.compress(
          o => objectStorage.store(StoredObjects.Blob(o)),
          t =>
              objectStorage.store(
                StoredObjects.Tree(t.view.mapValues(_.id).toMap)
              )
        )

        val commitId = objectStorage.store(
          StoredObjects.Commit(
            head.get.toList,
            treeId,
            meta
          )
        )

        head.set(Some(commitId))
        updateBranch(commitId)
        stagingArea.clear()
        commitId

    private def getTreeChildren(
        id: TreeId
    ): Map[PathElement, Ref[TreeId, BlobId]] =
        getTree(id).children.view
            .mapValues:
                case b: Blob    => BlobRef(writeBlob(b))
                case b: BlobRef => b
                case Tree(c)    => TreeRef(writeTree(Tree(c.toMap)))
                case t: TreeRef => t
            .toMap

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
        objectStorage.store(
          StoredObjects.Commit(
            commit.parentIds,
            commit.treeId,
            commit.meta
          )
        )

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
                case Tree(c)         => path -> writeTree(Tree(c.toMap))
                case b: Blob         => path -> writeBlob(b)
        val storedTree =
            StoredObjects.Tree[Id, PathElement](storedChildren)
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
                case StoredObjects.Tree(children) =>
                    val mapped = children.view
                        .mapValues(id => id -> objectStorage.get(id))
                        .mapValues:
                            case id -> Some(StoredObjects.Blob(_)) =>
                                RefLeaf(id)
                            case id -> Some(StoredObjects.Tree(_)) =>
                                RefNode(id)
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
          StoredObjects.Blob(blob.value)
        )

    /** @param blobId
      * @return
      */
    def getBlob(blobId: BlobId): this.Blob =
        objectStorage
            .get(blobId)
            .collect:
                case StoredObjects.Blob(obj) =>
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
            stagingArea.clear()
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
    def staged: Seq[ObjectPath[PathElement]] = stagingArea.staged.map(_.path)

    private def get(
        tree: StoredObjects.Tree[Id, PathElement],
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
                    case StoredObjects.Tree(c) => StoredObjects.Tree(c.toMap)
                .flatMap(t => get(t, objectPath.tail))

    private def headTreeId: Option[Id] =
        head.get
            .flatMap(objectStorage.get)
            .collect:
                case StoredObjects.Commit(_, treeId, _) =>
                    treeId

    private def getHeadTree: StoredObjects.Tree[Id, PathElement] =
        headTreeId
            .flatMap(objectStorage.get)
            .collect:
                case StoredObjects.Tree(c) => StoredObjects.Tree(c.toMap)
            .getOrElse(?!!)

    private def updateBranch(commitId: CommitId): Unit = currentBranch.get match
        case Some(label) => labelStorage.set(label, commitId)
        case None        => ()

    private def ?!! = throw IllegalStateException(
      "Panic, reached illegal state"
    )

    case class Commit(
        val parentIds: Seq[CommitId],
        val treeId: TreeId,
        val meta: Meta
    ) derives CanEqual

    // Alias generic RefTree
    type TreeItem = RefTree[TreeId, BlobId, PathElement, Obj]

    type TreeRef = RefNode[TreeId]
    val TreeRef = RefNode

    type Tree = ValueNode[TreeId, BlobId, PathElement, Obj]
    val Tree = ValueNode

    type Blob = ValueLeaf[Obj]
    val Blob = ValueLeaf

    type BlobRef = RefLeaf[BlobId]
    val BlobRef = RefLeaf

end Git

trait Head[Id]:
    def get: Option[Id]
    def set(id: Option[Id]): Unit
    final def unset(): Unit = set(None)
