package nyub.orthogit

import nyub.orthogit.id.Sha1.{ofHex, Sha1Id}
import nyub.orthogit.storage.LabelStorage
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.attribute.FileAttribute
import java.nio.file.StandardOpenOption
import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.git.StoredObjects
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit
import nyub.orthogit.id.Sha1
import java.util.zip.Deflater
import java.nio.ByteBuffer
import java.io.FileOutputStream

type FileObj = Seq[String]
type PathElement = String
type Label = String
type Id = Sha1Id
val stringSha1 = Sha1.deriveSha1Identifier[String]

class RealGitObjectStorage(private val gitRoot: Path)
    extends ObjectStorage[StoredObjects[FileObj, Id, PathElement], Id]:
    override def get(id: Id): Option[StoredObjects[FileObj, Id, PathElement]] =
        ???

    override def store(obj: StoredObjects[FileObj, Id, PathElement]): Id =
        obj match
            case Blob(obj) =>
                val all = obj.mkString("\n")
                writeObject(s"blob ${all.getBytes().length}\u0000${all}")
            case Tree(childrenIds) =>
                val all =
                    childrenIds.map((k, v) => s"${k}/${v.hex}").mkString("\n")
                writeObject(s"tree ${all}")
            case Commit(parentId, treeId) =>
                writeObject(
                  s"commit ${parentId.getOrElse("none")} ${treeId.hex}"
                )

    private def writeObject(content: String): Id =
        val id = stringSha1.id(content)
        val hexId = id.hex

        val deflater = java.util.zip.Deflater()
        val outPut = gitRoot.resolve(
          "objects",
          hexId.substring(0, 2),
          hexId.substring(2)
        )
        Files.createDirectories(outPut.getParent())
        FileOutputStream(outPut.toFile()).use: os =>
            val byteBuffer =
                ByteBuffer.allocate(2048)

            deflater.setInput(content.getBytes())
            deflater.finish()
            while deflater.getBytesRead() < content.length() do
                val written = deflater.deflate(byteBuffer.clear())
                os.write(byteBuffer.array(), 0, written)

            deflater.end()
            os.flush()
            id

    extension [T <: AutoCloseable](resource: T)
        private def use[R](exec: T => R): R =
            try
                exec(resource)
            finally
                resource.close()

class RealGitLabelStorage(private val gitRoot: Path)
    extends LabelStorage[String, Id]:
    private val labelsRoot = gitRoot.resolve("refs", "heads")
    override def get(label: String): Option[Id] =
        val labelFile = labelsRoot.resolve(label)
        if labelFile.toFile().isFile() then
            Some(ofHex(Files.readString(labelFile)))
        else None

    override def set(label: String, obj: Id): Unit =
        val labelFile = labelsRoot.resolve(label)
        Files.createDirectories(labelFile.getParent()): @annotation.nowarn(
          "msg=discarded"
        )
        Files.writeString(
          labelFile,
          obj.hex,
          StandardOpenOption.CREATE
        ): @annotation.nowarn("msg=discarded")

    override def delete(label: String): Option[Id] =
        val labelFile = labelsRoot.resolve(label)
        val id = get(label)
        if id.isDefined then Files.delete(labelFile)
        id
