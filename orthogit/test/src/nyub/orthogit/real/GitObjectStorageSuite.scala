package nyub.orthogit.real

import nyub.assert.AssertExtensions
import java.nio.file.Path
import java.nio.file.Files
import nyub.orthogit.git.StoredObjects
import nyub.orthogit.real.GitObjectEncoding.GitObject
import nyub.orthogit.experiment.Zlib

class GitObjectStorageSuite extends munit.FunSuite with AssertExtensions:
    private val gitRoot = FunFixture[Path](
      setup = test => Files.createTempDirectory("testGit"),
      teardown = path => rmRf(path)
    )

    gitRoot.test("Tree entry modes"): root =>
        val storage = GitObjectStorage(root)
        val blobId = storage.store(StoredObjects.Blob(Seq.empty))
        val subTreeId =
            storage.store(StoredObjects.Tree(Map("subBlob" -> blobId)))
        val treeId = storage.store(
          StoredObjects.Tree(Map("blob" -> blobId, "subTree" -> subTreeId))
        )
        val treeFile =
            root.resolve("objects", treeId.hex.take(2), treeId.hex.substring(2))
        val decodedTreeFile =
            GitObjectEncoding.decode(Zlib.decompressFile(treeFile))
        decodedTreeFile matches:
            case GitObject.Tree(children) =>
                children isEqualTo Seq(
                  GitObjectEncoding.TreeEntry(GitMode.File, "blob", blobId),
                  GitObjectEncoding.TreeEntry(
                    GitMode.Directory,
                    "subTree",
                    subTreeId
                  )
                )
                true

    private def rmRf(path: Path): Unit =
        val file = path.toFile()
        if file.isDirectory() then
            val children = file.list().map(path.resolve(_))
            children.foreach(rmRf)
        Files.delete(path)

end GitObjectStorageSuite
