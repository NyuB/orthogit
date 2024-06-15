package nyub.orthogit.real

import nyub.orthogit.git.GitSuite
import nyub.orthogit.id.Sha1.Sha1Id
import scala.collection.immutable.ArraySeq
import nyub.orthogit.git.Git
import java.nio.file.Files

class RealGitSuite
    extends GitSuite[Seq[Byte], Sha1Id, String, GitCommitMetadata]:
    override val differentObjects: (Seq[Byte], Seq[Byte]) =
        ("AAA".unsafeWrapped, "BBB".unsafeWrapped)

    override val differentPaths: (String, String) = ("pathA", "pathB")
    override val someMetadata: GitCommitMetadata = GitCommitMetadata(
      CommitterInfo("Author", "author@mail.com", 123456L, "+0200"),
      CommitterInfo("Committer", "committer@mail.com", 123456L, "+0200"),
      "Message"
    )

    override val someObject: Seq[Byte] = "ZZZ".unsafeWrapped
    override val somePath: String = "somePath"
    override def initializeGit()
        : Git[Seq[Byte], Sha1Id, String, GitCommitMetadata] =
        val temp = Files.createTempDirectory(".testGit")
        RealGit(temp)

    extension (s: String)
        private def unsafeWrapped = ArraySeq.unsafeWrapArray(s.getBytes())

end RealGitSuite
