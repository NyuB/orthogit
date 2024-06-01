package nyub.orthogit.real

import nyub.orthogit.id.Sha1
import nyub.orthogit.id.Sha1.Sha1Id
import scala.collection.immutable.ArraySeq
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

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
        case ObjectFormat.Tree(children) =>
            val content = children
                .flatMap: e =>
                    e.mode.getBytes() ++ (' '.toByte +: e.path
                        .getBytes()) ++ ('\u0000'.toByte +: e.id.bytes)
            ArraySeq.unsafeWrapArray(
              s"tree ${content.size}\u0000".getBytes()
            ) ++ content

    def decode(bytes: Seq[Byte]): ObjectFormat =
        if bytes.startsWith(BLOB_PREFIX) then
            ObjectFormat.Blob(extractContent(bytes, BLOB_PREFIX))
        else if bytes.startsWith(COMMIT_PREFIX) then ???
        else if bytes.startsWith(TREE_PREFIX) then
            parseTree(extractContent(bytes, TREE_PREFIX))
        else ???

    private def parseTree(content: Seq[Byte]): ObjectFormat.Tree =
        val entries = ArrayBuffer[TreeEntry]()
        var indexStart = 0
        var indexEnd = indexStart
        while indexStart < content.size do
            while content(indexEnd) != ' ' do indexEnd += 1
            val mode = String(
              content.slice(indexStart, indexEnd).toArray,
              StandardCharsets.UTF_8
            )
            indexStart = indexEnd + 1
            while content(indexEnd) != '\u0000' do indexEnd += 1
            val path = String(
              content.slice(indexStart, indexEnd).toArray,
              StandardCharsets.UTF_8
            )
            indexStart = indexEnd + 1
            indexEnd = indexStart + 20
            val sha1 =
                Sha1.ofBytes(content.slice(indexStart, indexEnd))
            indexStart = indexEnd
            entries.addOne(
              TreeEntry(
                mode,
                path,
                sha1
              )
            )
        ObjectFormat.Tree(entries.toSeq)

    private def extractContent(bytes: Seq[Byte], prefix: String): Seq[Byte] =
        val lstart = prefix.length()
        var lend = lstart
        while bytes(lend) != '\u0000' do lend += 1
        val len = String(bytes.slice(lstart, lend).toArray).toInt
        bytes.slice(lend + 1, lend + 1 + len)

    extension (s: String)
        private def unsafeWrapped = ArraySeq.unsafeWrapArray(s.getBytes())

end GitObjectEncoding
