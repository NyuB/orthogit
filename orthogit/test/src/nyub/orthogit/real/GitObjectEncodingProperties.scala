package nyub.orthogit.real

import nyub.assert.AssertExtensions
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._

import scala.collection.immutable.ArraySeq
import java.nio.charset.StandardCharsets

import nyub.orthogit.real.GitObjectEncoding.{CommitterInfo, GitObject}
import nyub.orthogit.id.Sha1
import nyub.orthogit.id.Sha1.Sha1Id

class GitObjectEncodingProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions:

    override def scalaCheckInitialSeed =
        "L6ILXuhXQyauaiyKhTyfFc1GmpmQ5Hr6ayJNTUeR2-G="

    given Arbitrary[GitObject] = Arbitrary(
      Gen.frequency(1 -> blobGen, 1 -> treeGen, 1 -> commitGen)
    )

    property("Encode then decode is identity"):
        forAll: (obj: GitObject) =>
            val encoded = GitObjectEncoding.encode(obj)
            val decoded = GitObjectEncoding.decode(encoded)
            decoded isEqualTo obj

    private val bytesGen: Gen[Seq[Byte]] = Gen.asciiStr.map(str =>
        ArraySeq.unsafeWrapArray(str.getBytes(StandardCharsets.UTF_8))
    )

    private val blobGen: Gen[GitObject.Blob] =
        bytesGen.map(GitObject.Blob(_))

    private val sha1Gen: Gen[Sha1Id] =
        Gen.stringOfN(40, Gen.hexChar).map(Sha1.ofHex)

    private val treeEntryGen: Gen[GitObjectEncoding.TreeEntry] =
        sha1Gen.flatMap: id =>
            Gen.nonEmptyStringOf(Gen.alphaNumChar)
                .flatMap: path =>
                    Gen.oneOf(Seq("40000", "100644"))
                        .map: mode =>
                            GitObjectEncoding.TreeEntry(mode, path, id)

    private val treeGen: Gen[GitObject.Tree] =
        Gen.listOf(treeEntryGen)
            .map: entries =>
                GitObject.Tree(entries)

    private val committerInfoGen: Gen[CommitterInfo] =
        Gen.stringOf(Gen.alphaNumChar)
            .filterNot(_.isBlank())
            .flatMap: name =>
                Gen.stringOf(Gen.alphaNumChar)
                    .filterNot(_.isBlank())
                    .flatMap: mail =>
                        Gen.long.map: ts =>
                            CommitterInfo(name, mail, ts, "+0000")

    private val commitGen: Gen[GitObject.Commit] = Gen
        .listOf(sha1Gen)
        .flatMap: parents =>
            sha1Gen.flatMap: treeId =>
                committerInfoGen.flatMap: author =>
                    committerInfoGen.flatMap: commiter =>
                        GitObject.Commit(
                          treeId,
                          parents,
                          author,
                          commiter,
                          "AAA"
                        )

end GitObjectEncodingProperties
