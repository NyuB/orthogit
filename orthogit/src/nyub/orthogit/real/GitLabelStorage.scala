package nyub.orthogit.real

import nyub.orthogit.storage.LabelStorage
import nyub.orthogit.id.Sha1.Sha1Id
import java.nio.file.Path
import java.nio.file.Files
import nyub.orthogit.id.Sha1

class GitLabelStorage(gitRoot: Path) extends LabelStorage[String, Sha1Id]:
    private val refRoots: Path = gitRoot.resolve("refs", "heads")
    override def get(label: String): Option[Sha1Id] =
        val refFile = refRoots.resolve(label)
        if refFile.toFile().isFile() then
            val hexSha1 = Files.readString(refFile).replace("\n", "")
            Some(Sha1.ofHex(hexSha1))
        else None

    override def set(label: String, obj: Sha1Id): Unit =
        throw UnsupportedOperationException("Read only")
    // val refFile = refRoots.resolve(label)
    // val refContent = s"${obj.hex}\n"
    // Files.writeString(refFile, refContent): @annotation.nowarn("msg=discarded")

    override def delete(label: String): Option[Sha1Id] =
        throw UnsupportedOperationException("Read only")
    // val res = get(label)
    // val refFile = refRoots.resolve(label)
    // if res.isDefined then
    //     Files.delete(refFile)

    // res
