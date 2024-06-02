package nyub.orthogit.git

import nyub.orthogit.git.StoredObjects.{Blob, Commit, Tree}
import nyub.orthogit.storage.LabelStorage
import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.orthogit.id.Identifier
import nyub.orthogit.id.Sha1.deriveSha1Identifier

type TestObj = String
type TestPath = String
type TestId = Sha1Id
type TestLabel = String
type TestMeta = String

class TestGit extends Git[TestObj, TestId, TestLabel, TestPath, TestMeta]:
    override protected val currentBranch: Head[TestLabel] =
        MutableOption[TestLabel]

    override protected val head: Head[TestId] = MutableOption[TestId]
    override protected val labelStorage: LabelStorage[String, Sha1Id] =
        LabelStorage.InMemory()

    override protected val objectStorage: ObjectStorage[
      StoredObjects[String, Sha1Id, String, TestMeta],
      Sha1Id
    ] =
        ObjectStorage.InMemory(using TestIdentifier)()

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
            case Commit(parentId, treeId, msg) =>
                stringSha1.id(
                  s"commit ${parentId.map(_.hex).getOrElse("none")} ${treeId.hex} ${msg}"
                )
