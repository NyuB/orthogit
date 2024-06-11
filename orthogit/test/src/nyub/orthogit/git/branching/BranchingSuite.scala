package nyub.orthogit.git.branching

import nyub.assert.AssertExtensions
import nyub.orthogit.git.TestGit
import nyub.orthogit.git.TestMeta
import nyub.orthogit.git.ObjectPath
import nyub.orthogit.git.TestPath
import nyub.orthogit.git.TestLabel
import nyub.orthogit.git.staging.StagedObject

@annotation.nowarn("msg=unused value")
class BranchingSuite extends munit.FunSuite with AssertExtensions:
    private val someMetadata: TestMeta = "Message"
    private val somePath: ObjectPath[TestPath] =
        ObjectPath(Seq("leading"), "trailing.path")

    private val someLabel: TestLabel = "test-branch"

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
        git.getObject(somePath) isEqualTo Some(initialContent)
        git.checkoutLabel(someLabel)
        git.getObject(somePath) isEqualTo Some(updatedContent)

end BranchingSuite
