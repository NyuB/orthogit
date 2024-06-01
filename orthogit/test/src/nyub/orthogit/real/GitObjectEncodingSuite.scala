package nyub.orthogit.real

import org.scalacheck.Prop._
import org.scalacheck.Gen
import nyub.assert.AssertExtensions
import org.scalacheck.Arbitrary
import java.nio.charset.StandardCharsets
import scala.collection.immutable.ArraySeq
import java.nio.file.Files
import java.nio.file.Paths
import nyub.orthogit.id.Sha1
import nyub.orthogit.id.Sha1.Sha1Id

class GitObjectEncodingTests extends munit.FunSuite with AssertExtensions:
    test("Decode blob object"):
        val content = Files.readAllBytes(
          Paths.get("orthogit/test/resources/blob_obj")
        )
        val decoded =
            GitObjectEncoding.decode(content.bytes)
        val expected = Files
            .readAllBytes(Paths.get("orthogit/test/resources/blob_obj_content"))
            .bytes
        decoded isEqualTo GitObjectEncoding.ObjectFormat.Blob(
          expected
        )

    test("Decode tree object"):
        val content = Files.readAllBytes(
          Paths.get("orthogit/test/resources/tree_obj")
        )
        val decoded =
            GitObjectEncoding.decode(content.bytes)
        decoded isEqualTo GitObjectEncoding.ObjectFormat.Tree(
          Seq(
            GitObjectEncoding.TreeEntry(
              "100644",
              ".gitignore",
              Sha1.ofHex("7c73c1d94bc9ab12cc8c2afdbf37e5645c618979")
            ),
            GitObjectEncoding.TreeEntry(
              "100644",
              ".mill-version",
              Sha1.ofHex("12edb292a21201f759d83a3875c05901d56369c9")
            ),
            GitObjectEncoding.TreeEntry(
              "100644",
              ".scalafmt.conf",
              Sha1.ofHex("6e9e632ea51bfab45a95f270d7e9971ced82046a")
            ),
            GitObjectEncoding.TreeEntry(
              "100644",
              "Makefile",
              Sha1.ofHex("f599a7188d392064f15b17e5cb4dd62b2ce85c9b")
            ),
            GitObjectEncoding.TreeEntry(
              "40000",
              "assert_extensions",
              Sha1.ofHex("63de18b1d68aa20fca4e2351a4331b51950815c8")
            ),
            GitObjectEncoding.TreeEntry(
              "100644",
              "build.sc",
              Sha1.ofHex("e7abe0b1f1defd9ce1f292beecdc821cb1908564")
            ),
            GitObjectEncoding.TreeEntry(
              "100644",
              "millw.bat",
              Sha1.ofHex("caff9a58fbf8d3a54cf317c970113b81ba6bb1ef")
            ),
            GitObjectEncoding.TreeEntry(
              "40000",
              "orthogit",
              Sha1.ofHex("df8b34bb04991f2796e06743717038878ef36321")
            )
          )
        )

    extension (arr: Array[Byte])
        private def bytes: Seq[Byte] = ArraySeq.unsafeWrapArray(arr)

    extension (s: String) def bytes: Seq[Byte] = s.getBytes().bytes
end GitObjectEncodingTests

class GitObjectEncodingProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions:

    override def scalaCheckInitialSeed =
        "L6ILXuhXQyauaiyKhTyfFc1GmpmQ5Hr6ayJNTUeR2-G="

    given Arbitrary[GitObjectEncoding.ObjectFormat] = Arbitrary(
      Gen.frequency(1 -> blobGen, (1 -> treeGen))
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

end GitObjectEncodingProperties
