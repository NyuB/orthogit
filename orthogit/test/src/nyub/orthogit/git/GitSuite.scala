package nyub.orthogit.git

import nyub.assert.AssertExtensions
import munit.diff.Printer
import nyub.orthogit.id.Sha1

class GitSuite extends munit.FunSuite with AssertExtensions:
    private val someMetadata: TestMeta = "Message"
    private val someObject: TestObj = "Object"

    override def printer: Printer = Printer:
        case s: Seq[?] if s.size == 20 && s(0).isInstanceOf[Byte] =>
            Sha1.ofBytes(s.asInstanceOf[Seq[Byte]]).hex

    test("Commit one object, checkout, retrieve object and commit metadata"):
        val git = TestGit.Core()

        val treeId =
            git.writeTree(git.Tree(Map("object" -> git.Blob(someObject))))
        val commitId =
            git.writeCommit(git.Commit(Seq.empty, treeId, someMetadata))
        git.checkout(commitId)

        git.getObject(ObjectPath("object")) isEqualTo Some(someObject)
        git.getCommit(commitId) matches:
            case git.Commit(Seq(), _, meta) =>
                meta == someMetadata

    test("Write an empty tree, get it back"):
        val git = TestGit.Core()
        val treeId = git.writeTree(git.Tree(Map.empty))
        git.getTree(treeId) isEqualTo git.Tree(Map.empty)

    test("Write an object at tree root, get it back"):
        val git = TestGit.Core()
        val blobId = git.writeBlob(git.Blob(""))
        val treeId = git.writeTree(git.Tree(Map("" -> git.BlobRef(blobId))))
        git.getTree(treeId) isEqualTo git.Tree(Map("" -> git.BlobRef(blobId)))

    test("Write two objects with same path prefix, commit, get each object"):
        val git = TestGit.Core()

        val a = "AAA"
        val b = "BBB"

        val treeId = git.writeTree(
          git.Tree(
            Map(
              "Shared" -> git.Tree(
                Map("A.txt" -> git.Blob(a), "B.txt" -> git.Blob(b))
              )
            )
          )
        )
        val commitId =
            git.writeCommit(git.Commit(Seq.empty, treeId, someMetadata))
        git.checkout(commitId)

        val pathA = ObjectPath(Seq("Shared"), "A.txt")
        val pathB = ObjectPath(Seq("Shared"), "B.txt")
        git.getObject(pathA) isEqualTo Some(a)
        git.getObject(pathB) isEqualTo Some(b)

end GitSuite
