package nyub.orthogit.real

case class GitCommitMetadata(
    val author: CommitterInfo,
    val committer: CommitterInfo,
    val message: String
)
