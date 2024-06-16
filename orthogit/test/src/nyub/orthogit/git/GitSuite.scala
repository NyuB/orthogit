package nyub.orthogit.git

import nyub.assert.AssertExtensions
import munit.diff.Printer
import nyub.orthogit.id.Sha1

@annotation.nowarn("msg=unused value")
@annotation.nowarn("msg=transitively initialized")
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

    test("log is empty when head points to no commit"):
        val git = initializeGit()
        git.log isEqualTo Seq.empty

    test("after writing one commit, log first element is the commit id"):
        val git = initializeGit()
        val commitId = git.commit(git.Tree(Map.empty), someMetadata)
        git.log isEqualTo Seq(commitId)

    test(
      "after writing two commits, log first element is the last commit id and second element is the first commit id"
    ):
        val git = initializeGit()
        val firstCommitId = git.commit(git.Tree(Map.empty), someMetadata)
        val secondCommitId = git.commit(git.Tree(Map.empty), someMetadata)
        git.log isEqualTo Seq(secondCommitId, firstCommitId)

    test(
      "after writing one commit with two parents, log second element is the first parent"
    ):
        val git = initializeGit()

        val oneTreeId = git.writeTree(git.Tree(Map.empty))
        val secondTreeId =
            git.writeTree(git.Tree(Map(somePath -> git.TreeRef(oneTreeId))))

        val firstCommitId =
            git.writeCommit(git.Commit(Seq.empty, oneTreeId, someMetadata))
        val secondCommitId =
            git.writeCommit(git.Commit(Seq.empty, secondTreeId, someMetadata))
        val thirdCommitId = git.writeCommit(
          git.Commit(
            Seq(firstCommitId, secondCommitId),
            secondTreeId,
            someMetadata
          )
        )

        git.log(thirdCommitId) isEqualTo Seq(thirdCommitId, firstCommitId)

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
