package nyub.orthogit.real

import nyub.assert.AssertExtensions
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._
import scala.collection.immutable.ArraySeq
import nyub.orthogit.id.Sha1.Sha1Id
import java.nio.charset.StandardCharsets
import nyub.orthogit.id.Sha1

class GitObjectEncodingProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions:

    override def scalaCheckInitialSeed =
        "L6ILXuhXQyauaiyKhTyfFc1GmpmQ5Hr6ayJNTUeR2-G="

    given Arbitrary[GitObjectEncoding.ObjectFormat] = Arbitrary(
      Gen.frequency(1 -> blobGen, 1 -> treeGen, 1 -> commitGen)
    )

    property("Encode then decode is identity"):
        forAll: (obj: GitObjectEncoding.ObjectFormat) =>
            val encoded = GitObjectEncoding.encode(obj)
            val decoded = GitObjectEncoding.decode(encoded)
            decoded isEqualTo obj

    private val bytesGen: Gen[Seq[Byte]] = Gen.asciiStr.map(str =>
        ArraySeq.unsafeWrapArray(str.getBytes(StandardCharsets.UTF_8))
    )

    private val blobGen: Gen[GitObjectEncoding.ObjectFormat.Blob] =
        bytesGen.map(GitObjectEncoding.ObjectFormat.Blob(_))

    private val sha1Gen: Gen[Sha1Id] =
        Gen.stringOfN(40, Gen.hexChar).map(Sha1.ofHex)

    private val treeEntryGen: Gen[GitObjectEncoding.TreeEntry] =
        sha1Gen.flatMap: id =>
            Gen.nonEmptyStringOf(Gen.alphaNumChar)
                .flatMap: path =>
                    Gen.oneOf(Seq("40000", "100644"))
                        .map: mode =>
                            GitObjectEncoding.TreeEntry(mode, path, id)

    private val treeGen: Gen[GitObjectEncoding.ObjectFormat.Tree] =
        Gen.listOf(treeEntryGen)
            .map: entries =>
                GitObjectEncoding.ObjectFormat.Tree(entries)

    private val commitGen: Gen[GitObjectEncoding.ObjectFormat.Commit] = Gen
        .listOf(sha1Gen)
        .flatMap: parents =>
            sha1Gen.map: treeId =>
                GitObjectEncoding.ObjectFormat.Commit(treeId, parents)

end GitObjectEncodingProperties
