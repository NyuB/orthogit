package nyub.orthogit.git

import nyub.assert.AssertExtensions
import munit.diff.Printer
import nyub.orthogit.id.Sha1

@annotation.nowarn("msg=unused value")
trait GitSuite[Obj, Id, PathElement, Meta](using
    CanEqual[PathElement, PathElement],
    CanEqual[Obj, Obj],
    CanEqual[Meta, Meta]
) extends munit.FunSuite
    with AssertExtensions:
    val someMetadata: Meta
    val someObject: Obj
    val somePath: PathElement
    val differentObjects: (Obj, Obj)
    val differentPaths: (PathElement, PathElement)
    def initializeGit(): Git[Obj, Id, PathElement, Meta]

    test("Commit one object, retrieve object and commit metadata"):
        val git = initializeGit()

        val commitId =
            git.commit(
              git.Tree(Map(somePath -> git.Blob(someObject))),
              someMetadata
            )

        git.getObject(ObjectPath(somePath)) isEqualTo Some(someObject)
        git.getCommit(commitId) matches:
            case git.Commit(Seq(), _, meta) =>
                meta == someMetadata

    test("Commit two objects with same path prefix, get each object"):
        val git = initializeGit()

        val (a, b) = differentObjects
        val (pathA, pathB) = differentPaths
        val sharedPath = somePath

        git.commit(
          git.Tree(
            Map(
              sharedPath -> git.Tree(
                Map(pathA -> git.Blob(a), pathB -> git.Blob(b))
              )
            )
          ),
          someMetadata
        )

        val fullPathA = ObjectPath(Seq(sharedPath), pathA)
        val fullPathB = ObjectPath(Seq(sharedPath), pathB)

        git.getObject(fullPathA) isEqualTo Some(a)
        git.getObject(fullPathB) isEqualTo Some(b)

    test("Write an empty tree, get it back"):
        val git = initializeGit()
        val treeId = git.writeTree(git.Tree(Map.empty))
        git.getTree(treeId) isEqualTo git.Tree(Map.empty)

    test("Write an object at tree root, get it back"):
        val git = initializeGit()
        val blobId = git.writeBlob(git.Blob(someObject))
        val treeId =
            git.writeTree(git.Tree(Map(somePath -> git.BlobRef(blobId))))
        git.getTree(treeId) isEqualTo git.Tree(
          Map(somePath -> git.BlobRef(blobId))
        )

end GitSuite

class TestGitSuite
    extends GitSuite[TestObj, TestId, TestPath, TestMeta]
    with AssertExtensions:
    override val someMetadata: TestMeta = "Message"
    override val someObject: TestObj = "Object"
    override val differentObjects: (TestObj, TestObj) = ("a", "b")
    override val differentPaths: (TestPath, TestPath) = ("A.txt", "B.txt")
    override val somePath: TestPath = "path"
    override def initializeGit(): Git[TestObj, TestId, TestPath, TestMeta] =
        TestGit.Core()

    override def printer: Printer = Printer:
        case s: Seq[?] if s.size == 20 && s(0).isInstanceOf[Byte] =>
            Sha1.ofBytes(s.asInstanceOf[Seq[Byte]]).hex

end TestGitSuite
