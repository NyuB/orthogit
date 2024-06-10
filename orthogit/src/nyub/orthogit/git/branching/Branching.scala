package nyub.orthogit.git.branching

import nyub.orthogit.git.Git
import nyub.orthogit.git.Head
import nyub.orthogit.storage.LabelStorage

trait Branching[Obj, Id, Label, PathElement, Meta]:
    git: Git[Obj, Id, PathElement, Meta] =>

    protected def labelStorage: LabelStorage[Label, git.CommitId]
    protected def currentBranch: Head[Label]

    /** [[Git.checkout]] to the commit indicated by `label` and positions the
      * current branch to `label`
      *
      * @param label
      *   label of the branch to go to
      * @return
      *   the head commit id after checkout
      */
    final def checkoutLabel(label: Label): CommitId =
        labelStorage.get(label) match
            case Some(id) =>
                checkout(id)
                currentBranch.set(Some(label))
                id
            case None => ???

    /** Makes the branch with name `label` point to commit `commitId`.
      *
      * @param name
      *   label of the branch. If it did not exist it is created, if it existed
      *   it is updated.
      * @param commitId
      *   commit id to point to
      */
    final def branch(name: Label, commitId: CommitId): Unit =
        labelStorage.set(name, commitId)

    final protected def updateBranch(commitId: CommitId): Unit =
        currentBranch.get match
            case Some(label) => labelStorage.set(label, commitId)
            case None        => ()
