package nyub.orthogit.real

import nyub.assert.AssertExtensions
import scala.collection.immutable.ArraySeq
import java.nio.file.Files
import java.nio.file.Paths
import nyub.orthogit.id.Sha1
import nyub.orthogit.real.GitObjectEncoding.GitObject

class GitObjectEncodingSuite extends munit.FunSuite with AssertExtensions:
    test("Decode blob object"):
        val content = Files.readAllBytes(
          Paths.get("orthogit/test/resources/blob_obj")
        )
        val decoded =
            GitObjectEncoding.decode(content.bytes)
        val expected = Files
            .readAllBytes(Paths.get("orthogit/test/resources/blob_obj_content"))
            .bytes
        decoded isEqualTo GitObject.Blob(
          expected
        )

    test("Decode tree object"):
        val content = Files.readAllBytes(
          Paths.get("orthogit/test/resources/tree_obj")
        )
        val decoded =
            GitObjectEncoding.decode(content.bytes)
        decoded isEqualTo GitObject.Tree(
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

    test("Decode commit object"):
        val content = Files
            .readAllBytes(Paths.get("orthogit/test/resources/commit_obj"))
            .bytes

        GitObjectEncoding.decode(
          content
        ) isEqualTo GitObject.Commit(
          Sha1.ofHex("854954518f639d2698107018ee6a7350ce22507d"),
          Seq(Sha1.ofHex("857af98c7592434e2f877c6163219d61aadefec2")),
          CommitterInfo(
            "Brice Decaestecker",
            "brice.decaestecker@gmx.fr",
            1717252829L,
            "+0200"
          ),
          CommitterInfo(
            "Brice Decaestecker",
            "brice.decaestecker@gmx.fr",
            1717252829L,
            "+0200"
          ),
          "Split unit tests and property tests\n"
        )

    extension (arr: Array[Byte])
        private def bytes: Seq[Byte] = ArraySeq.unsafeWrapArray(arr)

    extension (s: String) def bytes: Seq[Byte] = s.getBytes().bytes
end GitObjectEncodingSuite
