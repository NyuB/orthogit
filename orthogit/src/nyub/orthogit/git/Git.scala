package nyub.orthogit.git

import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit
import nyub.orthogit.reftree.{RefLeaf, RefNode, RefTree, ValueLeaf, ValueNode}

trait Git[Obj, Id, PathElement, Meta](using
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

    protected def head: Head[CommitId]

    protected def onCheckout(from: Option[CommitId], to: CommitId): Unit

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
      * @param commitId
      *   id of the commit to move to
      */
    final def checkout(commitId: CommitId): Unit =
        objectStorage.get(commitId) match
            case Some(StoredObjects.Commit(_, treeId, _)) =>
                onCheckout(head.get, commitId)
                head.set(Some(commitId))
            case _ => ???

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
    final def getObject(objectPath: ObjectPath[PathElement]): Option[Obj] =
        get(getHeadTree, objectPath)

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

    final protected def headTreeId: Option[TreeId] =
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
