package nyub.orthogit.git.staging

import nyub.orthogit.reftree.ValueLeaf
import nyub.orthogit.reftree.insert
import nyub.orthogit.reftree.Ref
import nyub.orthogit.reftree.compress
import nyub.orthogit.git.Git
import nyub.orthogit.git.ObjectPath

trait Staging[Obj, Id, Label, PathElement, Meta](using
    CanEqual[PathElement, PathElement]
):
    git: Git[Obj, Id, Label, PathElement, Meta] =>

    protected def stagingArea
        : StagingArea[git.TreeId, git.BlobId, PathElement, Obj]

    /** Stages `obj`, adding it to the bulk of objects that would be part of a
      * [[Staging.commit]]. If there is already a staged object at this path, it
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
      *   [[Staging.add]] to stage objects before committing
      */
    def commit(meta: Meta): CommitId =
        val currentTree = headTreeId.map(TreeRef(_)).getOrElse(Tree(Map.empty))
        val updated = stagingArea.staged.foldLeft(currentTree): (t, s) =>
            t.insert(s.path.name, s.path.path, s.obj, getTreeChildren)
        val treeId = updated.compress(
          o => writeBlob(Blob(o)),
          t => writeTree(Tree(t))
        )

        val commitId = writeCommit(Commit(head.get.toList, treeId, meta))

        head.set(Some(commitId))
        updateBranch(commitId)
        stagingArea.clear()
        commitId

    /** @return
      *   all paths leading to objects staged for update
      */
    def staged: Seq[ObjectPath[PathElement]] = stagingArea.staged.map(_.path)

    private def getTreeChildren(
        id: git.TreeId
    ): Map[PathElement, Ref[git.TreeId, git.BlobId]] =
        getTree(id).children.view
            .mapValues:
                case b: Blob    => BlobRef(writeBlob(b))
                case b: BlobRef => b
                case Tree(c)    => TreeRef(writeTree(Tree(c.toMap)))
                case t: TreeRef => t
            .toMap
