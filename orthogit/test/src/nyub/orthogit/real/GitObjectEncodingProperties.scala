package nyub.orthogit.real

import nyub.assert.AssertExtensions
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.collection.immutable.ArraySeq
import java.nio.charset.StandardCharsets

import nyub.orthogit.real.GitObjectEncoding.GitObject
import nyub.orthogit.id.Sha1
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.assert.PropertiesExtensions

class GitObjectEncodingProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions
    with PropertiesExtensions:

    override def scalaCheckInitialSeed =
        "L6ILXuhXQyauaiyKhTyfFc1GmpmQ5Hr6ayJNTUeR2-G="

    given Arbitrary[GitObject] = Arbitrary(
      Gen.frequency(1 -> blobGen, 1 -> treeGen, 1 -> commitGen)
    )

    property("decoding is the inverse of encoding"):
        GitObjectEncoding.decode isTheInverseOf GitObjectEncoding.encode

    private def bytesGen: Gen[Seq[Byte]] = Gen.asciiStr.map(str =>
        ArraySeq.unsafeWrapArray(str.getBytes(StandardCharsets.UTF_8))
    )

    private def blobGen: Gen[GitObject.Blob] =
        bytesGen.map(GitObject.Blob(_))

    private def sha1Gen: Gen[Sha1Id] =
        Gen.stringOfN(40, Gen.hexChar).map(Sha1.ofHex)

    private def treeEntryGen: Gen[GitObjectEncoding.TreeEntry] =
        sha1Gen.flatMap: id =>
            Gen.nonEmptyStringOf(Gen.alphaNumChar)
                .flatMap: path =>
                    Gen.oneOf(Seq(GitMode.File, GitMode.Directory))
                        .map: mode =>
                            GitObjectEncoding.TreeEntry(mode, path, id)

    private def treeGen: Gen[GitObject.Tree] =
        Gen.listOf(treeEntryGen)
            .map: entries =>
                GitObject.Tree(entries)

    private def commitGen: Gen[GitObject.Commit] = Gen
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
