package nyub.orthogit.git

import nyub.assert.AssertExtensions
import nyub.orthogit.id.Identifier
import nyub.orthogit.id.Sha1.{deriveSha1Identifier, Sha1Id}
import nyub.orthogit.git.StoredObjects.{Blob, Commit, Tree}
import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import scala.annotation

@annotation.nowarn("msg=unused value")
class GitSuite extends munit.FunSuite with AssertExtensions:
    private type TestObj = String
    private type TestPath = String
    private type TestId = Sha1Id
    private type TestLabel = String
    private type TestMeta = String
    private val someMetadata: TestMeta = "Message"
    private val someObject: TestObj = "Object"
    private val somePath: ObjectPath[TestPath] =
        ObjectPath(Seq("leading"), "trailing.path")

    private val someLabel: TestLabel = "test-branch"
    private val fileSha1 = deriveSha1Identifier[String]

    test("Add one object, commit, retrieve object and commit metadata"):
        val git = TestGit()

        git.add(StagedObject(somePath, someObject))
        val commitId = git.commit(someMetadata)

        git.get(somePath) isEqualTo Some(someObject)
        git.getCommit(commitId) matches:
            case git.Commit(None, _, meta) =>
                meta == someMetadata

    test("Add two objects with same path prefix, commit, get each object"):
        val git = TestGit()
        val pathA = ObjectPath(Seq("Shared"), "A.txt")
        val pathB = ObjectPath(Seq("Shared"), "B.txt")
        val a = "AAA"
        val b = "BBB"

        git.add(StagedObject(pathA, a))
        git.add(StagedObject(pathB, b))
        git.commit(someMetadata)

        git.get(pathA) isEqualTo Some(a)
        git.get(pathB) isEqualTo Some(b)

    test("Add two objects in two successive commits, get each object"):
        val git = TestGit()
        val pathA = ObjectPath(Seq.empty, "A.txt")
        val pathB = ObjectPath(Seq.empty, "B.txt")
        val a = "AAA"
        val b = "BBB"

        git.add(StagedObject(pathA, a))
        git.commit(someMetadata)
        git.add(StagedObject(pathB, b))
        git.commit(someMetadata)

        git.get(pathA) isEqualTo Some(a)
        git.get(pathB) isEqualTo Some(b)

    test("Staging area contains path after add"):
        val git = TestGit()
        git.add(StagedObject(somePath, someObject))
        git.staged isEqualTo Seq(somePath)

    test("Staging area is empty after commit"):
        val git = TestGit()
        git.add(StagedObject(somePath, someObject))

        git.commit(someMetadata)

        git.staged isEqualTo Seq.empty

    test("Staging area is empty after checkout"):
        val git = TestGit()
        val commitId = git.commit(someMetadata)
        git.branch(someLabel, commitId)
        def addSomeObject() = git.add(StagedObject(somePath, someObject))

        // Simple checkout
        addSomeObject()
        git.checkout(commitId)
        git.staged isEqualTo Seq.empty

        // Label checkout
        addSomeObject()
        git.checkoutLabel(someLabel)
        git.staged isEqualTo Seq.empty

    test(
      "Commit one object, update it and commit, checkout each commit and get object"
    ):
        val git = TestGit()
        val initialContent = "AAA"
        val updatedContent = "ZZZ"

        git.add(StagedObject(somePath, initialContent))
        val initialCommit = git.commit(someMetadata)
        git.add(StagedObject(somePath, updatedContent))
        val secondCommit = git.commit(someMetadata)

        git.get(somePath) isEqualTo Some(updatedContent)
        git.checkout(initialCommit)
        git.get(somePath) isEqualTo Some(initialContent)
        git.checkout(secondCommit)
        git.get(somePath) isEqualTo Some(updatedContent)

    test(
      "Commit version#1, make branch, commit version #2, checkout version#1, checkout branch, get version#2"
    ):
        val git = TestGit()
        val initialContent = "AAA"
        val updatedContent = "ZZZ"

        git.add(StagedObject(somePath, initialContent))
        val initialCommit = git.commit(someMetadata)
        git.branch(someLabel, initialCommit)
        git.checkoutLabel(someLabel)
        git.add(StagedObject(somePath, updatedContent))
        git.commit(someMetadata)

        git.checkout(initialCommit)
        git.get(somePath) isEqualTo Some(initialContent)
        git.checkoutLabel(someLabel)
        git.get(somePath) isEqualTo Some(updatedContent)

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

        override def id(
            obj: StoredObjects[TestObj, TestId, TestPath, TestMeta]
        ): TestId =
            obj match
                case Blob(obj) => fileSha1.id(s"blob ${obj.length}\u0000${obj}")
                case Tree(childrenIds) =>
                    val cId = childrenIds.toList
                        .sortBy(_._1)
                        .foldLeft(fileSha1.id("").hex): (acc, item) =>
                            val (path, objId) = item
                            fileSha1.id(s"${acc}${path}/${objId}").hex
                    fileSha1.id(s"tree ${cId}")
                case Commit(parentId, treeId, msg) =>
                    fileSha1.id(
                      s"commit ${parentId.map(_.hex).getOrElse("none")} ${treeId.hex} ${msg}"
                    )

end GitSuite
