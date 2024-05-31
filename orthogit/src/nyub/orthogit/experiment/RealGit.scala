package nyub.orthogit.experiment

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
import java.io.FileOutputStream
import java.io.FileInputStream
import java.util.zip.InflaterInputStream
import java.util.zip.DeflaterOutputStream
import java.io.ByteArrayOutputStream

type FileObj = Seq[String]
type PathElement = String
type Label = String
type Id = Sha1Id
val stringSha1 = Sha1.deriveSha1Identifier[String]

class RealGitObjectStorage(private val gitRoot: Path)
    extends ObjectStorage[StoredObjects[FileObj, Id, PathElement], Id]:
    override def get(id: Id): Option[StoredObjects[FileObj, Id, PathElement]] =
        readObject(id).map(o => StoredObjects.Blob(Seq(o)))

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

    private def readObject(id: Id): Option[String] =
        val hex = id.hex
        val path =
            gitRoot.resolve("objects", hex.substring(0, 2), hex.substring(2))
        val isBuffer = Array.ofDim[Byte](512)
        val res = ByteArrayOutputStream()
        FileInputStream(path.toFile()).use: is =>
            InflaterInputStream(is).use: inflater =>
                var read = 0
                while read != -1 do
                    read = inflater.read(isBuffer, 0, isBuffer.length)
                    if read > 0 then res.write(isBuffer, 0, read)
        Some(String(res.toByteArray()))

    private def writeObject(content: String): Id =
        val id = stringSha1.id(content)
        val hexId = id.hex
        val outPut = gitRoot.resolve(
          "objects",
          hexId.substring(0, 2),
          hexId.substring(2)
        )
        Files.createDirectories(outPut.getParent())
        FileOutputStream(outPut.toFile()).use: os =>
            DeflaterOutputStream(os).use: deflater =>
                deflater.write(content.getBytes())
        id

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
