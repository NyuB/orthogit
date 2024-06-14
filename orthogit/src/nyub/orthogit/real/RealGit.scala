package nyub.orthogit.real

import nyub.orthogit.git.Git
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.orthogit.git.Head
import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.git.StoredObjects
import java.nio.file.Path
import nyub.orthogit.git.branching.Branching
import nyub.orthogit.storage.LabelStorage
import java.nio.file.Files

class RealGit(private val gitRoot: Path)
    extends Git[Seq[Byte], Sha1Id, String, GitCommitMetadata]
    with Branching[Seq[Byte], Sha1Id, String, String, GitCommitMetadata]:
    override protected val head: Head[CommitId] = HeadRef()
    override protected def objectStorage: ObjectStorage[StoredObjects[Seq[
      Byte
    ], Sha1Id, String, GitCommitMetadata], Sha1Id] =
        GitObjectStorage(gitRoot)

    override protected val currentBranch: Head[String] = BranchRef()

    override protected def labelStorage: LabelStorage[String, CommitId] =
        GitLabelStorage(gitRoot).map(
          _.asCommitId,
          commitId => Some(commitId)
        )

    override protected def onCheckout(
        from: Option[CommitId],
        to: CommitId
    ): Unit = println(s"Checked out ref ${to} from ref ${from}")

    private class HeadRef extends Head[CommitId]:
        override def get: Option[CommitId] =
            currentBranch.get.flatMap(labelStorage.get)

        override def set(id: Option[CommitId]): Unit =
            throw UnsupportedOperationException("Read only")

    private class BranchRef extends Head[String]:
        override def get: Option[String] =
            val headFile = gitRoot.resolve("HEAD")
            val headContent =
                Files.readAllLines(headFile).get(0).split("refs/heads/")(1)
            Some(headContent)

        override def set(id: Option[String]): Unit =
            throw UnsupportedOperationException("Read only")
