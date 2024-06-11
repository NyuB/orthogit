package nyub.orthogit.git.staging

import nyub.orthogit.git.TestGit
import nyub.orthogit.git.TestMeta
import nyub.orthogit.git.TestObj
import nyub.orthogit.git.ObjectPath
import nyub.orthogit.git.TestPath
import nyub.assert.AssertExtensions
import nyub.orthogit.git.TestLabel

@annotation.nowarn("msg=unused value")
class StagingSuite extends munit.FunSuite with AssertExtensions:
    private val someMetadata: TestMeta = "Message"
    private val someObject: TestObj = "Object"
    private val someLabel: TestLabel = "test-branch"
    private val somePath: ObjectPath[TestPath] =
        ObjectPath(Seq("leading"), "trailing.path")

    test("Add one object, commit, retrieve object and commit metadata"):
        val git = TestGit()

        git.add(StagedObject(somePath, someObject))
        val commitId = git.commit(someMetadata)

        git.getObject(somePath) isEqualTo Some(someObject)
        git.getCommit(commitId) matches:
            case git.Commit(Seq(), _, meta) =>
                meta == someMetadata

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

        git.getObject(pathA) isEqualTo Some(a)
        git.getObject(pathB) isEqualTo Some(b)

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

        git.getObject(somePath) isEqualTo Some(updatedContent)
        git.checkout(initialCommit)
        git.getObject(somePath) isEqualTo Some(initialContent)
        git.checkout(secondCommit)
        git.getObject(somePath) isEqualTo Some(updatedContent)

    test("Staging area contains path after add"):
        val git = TestGit()
        git.add(StagedObject(somePath, someObject))
        git.staged isEqualTo Seq(somePath)

    test(
      "Adding two object with the same path erases the first one from staged"
    ):
        val git = TestGit()
        val a = "AAA"
        val b = "BBB"
        git.add(StagedObject(somePath, a))
        git.add(StagedObject(somePath, b))

        git.staged isEqualTo Seq(somePath)
        git.commit(someMetadata)
        git.getObject(somePath) isEqualTo Some(b)

    test(
      "Adding two object with the second object path included in the first one erases the first one from staged"
    ):
        val git = TestGit()
        val a = "AAA"
        val b = "BBB"
        val pathA = ObjectPath(Seq.empty, "a")
        val pathB = ObjectPath(Seq("a"), "b")
        git.add(StagedObject(pathA, a))
        git.add(StagedObject(pathB, b))

        git.staged isEqualTo Seq(pathB)
        git.commit(someMetadata)
        git.getObject(pathA) isEqualTo None
        git.getObject(pathB) isEqualTo Some(b)

    test(
      "Adding two object with the first object path included in the second one erases the first one from staged"
    ):
        val git = TestGit()
        val a = "AAA"
        val b = "BBB"
        val pathA = ObjectPath(Seq("a"), "b")
        val pathB = ObjectPath(Seq.empty, "a")
        git.add(StagedObject(pathA, a))
        git.add(StagedObject(pathB, b))

        git.staged isEqualTo Seq(pathB)
        git.commit(someMetadata)
        git.getObject(pathA) isEqualTo None
        git.getObject(pathB) isEqualTo Some(b)

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

end StagingSuite
