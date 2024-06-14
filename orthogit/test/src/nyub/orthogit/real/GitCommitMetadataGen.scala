package nyub.orthogit.real

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

def gitCommitMetadataGen: Gen[GitCommitMetadata] = Gen
    .stringOf(summon[Arbitrary[Char]].arbitrary)
    .flatMap: msg =>
        committerInfoGen.flatMap: author =>
            committerInfoGen.map: committer =>
                GitCommitMetadata(author, committer, msg)
