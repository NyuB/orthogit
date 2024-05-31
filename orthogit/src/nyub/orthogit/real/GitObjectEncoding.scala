package nyub.orthogit.real

import nyub.orthogit.id.Sha1.Sha1Id
import scala.collection.immutable.ArraySeq
import java.nio.charset.StandardCharsets

object GitObjectEncoding:
    private val BLOB_PREFIX = "blob "
    private val TREE_PREFIX = "tree "
    private val COMMIT_PREFIX = "commit "

    enum ObjectFormat derives CanEqual:
        case Blob(content: Seq[Byte])
        case Commit(treeId: Sha1Id, parents: Seq[Sha1Id])
        case Tree(children: Seq[TreeEntry])

    case class TreeEntry(val mode: String, val path: String, val id: Sha1Id)

    def encode(obj: ObjectFormat): Seq[Byte] = obj match
        case ObjectFormat.Blob(content) =>
            s"blob ${content.size}\u0000${String(content.toArray, StandardCharsets.UTF_8)}".unsafeWrapped
        case ObjectFormat.Commit(treeId, parents) => ???
        case ObjectFormat.Tree(children)          => ???

    def decode(bytes: Seq[Byte]): ObjectFormat =
        if bytes.startsWith(BLOB_PREFIX) then
            ObjectFormat.Blob(extractContent(bytes, BLOB_PREFIX))
        else if bytes.startsWith(COMMIT_PREFIX) then ???
        else if bytes.startsWith(TREE_PREFIX) then ???
        else ???

    private def extractContent(bytes: Seq[Byte], prefix: String): Seq[Byte] =
        val lstart = prefix.length()
        var lend = lstart
        while bytes(lend) != '\u0000' do lend += 1
        bytes.slice(lend + 1, bytes.size)

    extension (s: String)
        private def unsafeWrapped = ArraySeq.unsafeWrapArray(s.getBytes())

end GitObjectEncoding
