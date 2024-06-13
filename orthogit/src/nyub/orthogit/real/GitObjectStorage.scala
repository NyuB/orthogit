package nyub.orthogit.real

import nyub.orthogit.storage.ObjectStorage
import nyub.orthogit.id.Sha1.Sha1Id
import java.nio.file.Path
import nyub.orthogit.experiment.Zlib
import nyub.orthogit.id.Sha1
import nyub.orthogit.git.StoredObjects
import nyub.orthogit.real.GitObjectEncoding.GitObject
import nyub.orthogit.git.StoredObjects.Blob
import nyub.orthogit.git.StoredObjects.Tree
import nyub.orthogit.git.StoredObjects.Commit

class GitObjectStorage(gitRoot: Path)
    extends ObjectStorage[
      StoredObjects[Seq[Byte], Sha1Id, String, GitCommitMetadata],
      Sha1Id
    ]:
    private val objectRoot = gitRoot.resolve("objects")
    override def get(
        id: Sha1Id
    ): Option[StoredObjects[Seq[Byte], Sha1Id, String, GitCommitMetadata]] =
        val hexRepr = id.hex
        val objectFile =
            objectRoot.resolve(hexRepr.take(2), hexRepr.substring(2))
        if objectFile.toFile().isFile() then
            val objectContent = Zlib.decompressFile(objectFile)
            GitObjectEncoding.decode(objectContent) match
                case GitObject.Blob(content) =>
                    Some(StoredObjects.Blob(content))
                case GitObject.Commit(
                      treeId,
                      parentIds,
                      author,
                      committer,
                      message
                    ) =>
                    Some(
                      StoredObjects.Commit(
                        parentIds,
                        treeId,
                        GitCommitMetadata(author, committer, message)
                      )
                    )
                case GitObject.Tree(children) =>
                    Some(
                      StoredObjects.Tree(
                        children.map(e => e.path -> e.id).toMap
                      )
                    )
        else None

    override def store(
        obj: StoredObjects[Seq[Byte], Sha1Id, String, GitCommitMetadata]
    ): Sha1Id =
        val content = obj match
            case Blob(obj) =>
                GitObjectEncoding.encode(GitObjectEncoding.GitObject.Blob(obj))
            case Tree(childrenIds) =>
                GitObjectEncoding.encode(
                  GitObjectEncoding.GitObject.Tree(
                    childrenIds.toList.map(e =>
                        GitObjectEncoding.TreeEntry("104", e._1, e._2)
                    )
                  )
                )
            case Commit(parentIds, treeId, metadata) =>
                GitObjectEncoding.encode(
                  GitObjectEncoding.GitObject.Commit(
                    treeId,
                    parentIds,
                    metadata.author,
                    metadata.committer,
                    metadata.message
                  )
                )

        val id = Sha1.ofBytes(content)
        val hexRepr = id.hex
        val objectFile =
            objectRoot.resolve(hexRepr.take(2), hexRepr.substring(2))
        Zlib.compressIntoFile(objectFile, content)
        id
