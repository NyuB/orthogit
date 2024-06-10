package nyub.orthogit.git

import nyub.orthogit.git.StoredObjects.{Blob, Commit, Tree}
import nyub.orthogit.storage.LabelStorage
import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.orthogit.id.Identifier
import nyub.orthogit.id.Sha1.deriveSha1Identifier
import staging.{Staging, StagingArea}
import nyub.orthogit.git.branching.Branching

type TestObj = String
type TestPath = String
type TestId = Sha1Id
type TestLabel = String
type TestMeta = String

class TestGit
    extends Git[TestObj, TestId, TestPath, TestMeta]
    with Staging[TestObj, TestId, TestPath, TestMeta]
    with Branching[TestObj, TestId, TestLabel, TestPath, TestMeta]:
    override protected val currentBranch: Head[TestLabel] =
        MutableOption[TestLabel]

    override protected val head: Head[this.CommitId] =
        MutableOption[this.CommitId]

    override protected val labelStorage: LabelStorage[String, CommitId] =
        LabelStorage.InMemory()

    override protected val objectStorage: ObjectStorage[
      StoredObjects[String, Sha1Id, String, TestMeta],
      Sha1Id
    ] =
        ObjectStorage.InMemory(using TestIdentifier)()

    override protected val stagingArea
        : StagingArea[TreeId, BlobId, TestPath, TestObj] =
        staging.StagingArea.InMemory[TreeId, BlobId, TestPath, TestObj]()

    override protected def onCheckout(
        from: Option[CommitId],
        to: CommitId
    ): Unit =
        stagingArea.clear()
        currentBranch.unset()

    override protected def onCommit(commitId: CommitId): Unit = updateBranch(
      commitId
    )

    private class MutableOption[T] extends Head[T]:
        private var headPointer: Option[T] = None
        override def get: Option[T] = headPointer
        override def set(id: Option[T]): Unit = headPointer = id

object TestIdentifier
    extends Identifier[
      StoredObjects[TestObj, TestId, TestPath, TestMeta],
      TestId
    ]:

    private val stringSha1 = deriveSha1Identifier[String]

    override def id(
        obj: StoredObjects[TestObj, TestId, TestPath, TestMeta]
    ): TestId =
        obj match
            case Blob(obj) => stringSha1.id(s"blob ${obj.length}\u0000${obj}")
            case Tree(childrenIds) =>
                val cId = childrenIds.toList
                    .sortBy(_._1)
                    .foldLeft(stringSha1.id("").hex): (acc, item) =>
                        val (path, objId) = item
                        stringSha1.id(s"${acc}${path}/${objId}").hex
                stringSha1.id(s"tree ${cId}")
            case Commit(parentIds, treeId, msg) =>
                val pId = parentIds
                    .map(_.hex)
                    .sorted
                    .foldLeft(stringSha1.id("").hex): (acc, item) =>
                        stringSha1.id(s"${acc}${item}").hex
                stringSha1.id(
                  s"commit ${pId} ${treeId.hex} ${msg}"
                )
