package nyub.orthogit.real

import org.scalacheck.Prop._
import org.scalacheck.Gen
import nyub.assert.AssertExtensions
import org.scalacheck.Arbitrary
import java.nio.charset.StandardCharsets
import scala.collection.immutable.ArraySeq
import java.nio.file.Files
import java.nio.file.Paths

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

    extension (arr: Array[Byte])
        private def bytes: Seq[Byte] = ArraySeq.unsafeWrapArray(arr)

    extension (s: String) def bytes: Seq[Byte] = s.getBytes().bytes
end GitObjectEncodingTests

class GitObjectEncodingProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions:

    given Gen[Seq[Byte]] = Gen.asciiStr.map(str =>
        ArraySeq.unsafeWrapArray(str.getBytes(StandardCharsets.UTF_8))
    )

    given blobGen(using
        bytes: Gen[Seq[Byte]]
    ): Gen[GitObjectEncoding.ObjectFormat] =
        bytes.map(GitObjectEncoding.ObjectFormat.Blob(_))

    given Arbitrary[GitObjectEncoding.ObjectFormat] = Arbitrary(blobGen)

    property("Encode o decode is identity"):
        forAll: (obj: GitObjectEncoding.ObjectFormat) =>
            val encoded = GitObjectEncoding.encode(obj)
            val decoded = GitObjectEncoding.decode(encoded)
            decoded isEqualTo obj

end GitObjectEncodingProperties
