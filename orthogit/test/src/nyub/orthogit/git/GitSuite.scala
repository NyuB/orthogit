package nyub.orthogit.git

import nyub.assert.AssertExtensions
import nyub.orthogit.id.Identifier
import nyub.orthogit.id.Sha1.{deriveSha1Identifier, Sha1Id}
import nyub.orthogit.git.StoredObjects.{Blob, Commit, Tree}
import nyub.orthogit.storage.{LabelStorage, ObjectStorage}
import scala.annotation

@annotation.nowarn("msg=unused value")
class GitSuite extends munit.FunSuite with AssertExtensions:
    private type TestFile = String
    private type TestPath = String
    private type TestId = Sha1Id
    private type TestLabel = String
    private val fileSha1 = deriveSha1Identifier[String]

    test("Write then get one object"):
        val git = TestGit()
        val path = ObjectPath(Seq(), "A.txt")
        val content = "AAA"
        git.add(StagedObject(path, content))
        git.commit()
        git.get(path) isEqualTo Some(content)

    test("Add two objects with same path prefix, commit, get each object"):
        val git = TestGit()
        val pathA = ObjectPath(Seq("Shared"), "A.txt")
        val pathB = ObjectPath(Seq("Shared"), "B.txt")
        val a = "AAA"
        val b = "BBB"

        git.add(StagedObject(pathA, a))
        git.add(StagedObject(pathB, b))
        git.commit()

        git.get(pathA) isEqualTo Some(a)
        git.get(pathB) isEqualTo Some(b)

    test("Add two objects in two successive commits, get each object"):
        val git = TestGit()
        val pathA = ObjectPath(Seq.empty, "A.txt")
        val pathB = ObjectPath(Seq.empty, "B.txt")
        val a = "AAA"
        val b = "BBB"

        git.add(StagedObject(pathA, a))
        git.commit()
        git.add(StagedObject(pathB, b))
        git.commit()

        git.get(pathA) isEqualTo Some(a)
        git.get(pathB) isEqualTo Some(b)

    test("Staging area is empty after commit"):
        val git = TestGit()
        val pathA = ObjectPath(Seq.empty, "A.txt")
        val a = "AAA"
        git.add(StagedObject(pathA, a))
        git.commit()
        git.staged isEqualTo Seq.empty[ObjectPath[String]]

    test(
      "Commit one object, update it and commit, checkout each commit and get object"
    ):
        val git = TestGit()
        val path = ObjectPath(Seq(), "A.txt")
        val initialContent = "AAA"
        val updatedContent = "ZZZ"

        git.add(StagedObject(path, initialContent))
        val initialCommit = git.commit()
        git.add(StagedObject(path, updatedContent))
        val secondCommit = git.commit()

        git.get(path) isEqualTo Some(updatedContent)
        git.checkout(initialCommit)
        git.get(path) isEqualTo Some(initialContent)
        git.checkout(secondCommit)
        git.get(path) isEqualTo Some(updatedContent)

    test(
      "Commit version#1, make branch, commit version #2, checkout version#1, checkout branch, get version#2"
    ):
        val git = TestGit()
        val path = ObjectPath(Seq(), "A.txt")
        val initialContent = "AAA"
        val updatedContent = "ZZZ"

        git.add(StagedObject(path, initialContent))
        val initialCommit = git.commit()
        git.branch("main", initialCommit)
        git.checkoutBranch("main")
        git.add(StagedObject(path, updatedContent))
        git.commit()

        git.checkout(initialCommit)
        git.get(path) isEqualTo Some(initialContent)
        git.checkoutBranch("main")
        git.get(path) isEqualTo Some(updatedContent)

    class TestGit extends Git[TestFile, TestId, TestLabel, TestPath]:
        override protected val currentBranch: Head[TestLabel] =
            MutableOption[TestLabel]

        override protected val head: Head[TestId] = MutableOption[TestId]
        override protected val labelStorage: LabelStorage[String, Sha1Id] =
            LabelStorage.InMemory()

        override protected val objectStorage
            : ObjectStorage[StoredObjects[String, Sha1Id, String], Sha1Id] =
            ObjectStorage.InMemory(using TestIdentifier)()

        private class MutableOption[T] extends Head[T]:
            private var headPointer: Option[T] = None
            override def get: Option[T] = headPointer
            override def set(id: Option[T]): Unit = headPointer = id

    object TestIdentifier
        extends Identifier[StoredObjects[TestFile, TestId, TestPath], TestId]:

        override def id(
            obj: StoredObjects[TestFile, TestId, TestPath]
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
                case Commit(parentId, treeId) =>
                    fileSha1.id(
                      s"commit ${parentId.map(_.hex).getOrElse("none")} ${treeId.hex}}"
                    )

end GitSuite
