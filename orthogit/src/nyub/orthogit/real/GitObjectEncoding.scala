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
    private val PARENT_PREFIX = "parent "
    private val AUTHOR_PREFIX = "author "

    enum ObjectFormat derives CanEqual:
        case Blob(content: Seq[Byte])
        case Commit(treeId: Sha1Id, parents: Seq[Sha1Id])
        case Tree(children: Seq[TreeEntry])

    case class TreeEntry(val mode: String, val path: String, val id: Sha1Id)

    def encode(obj: ObjectFormat): Seq[Byte] = obj match
        case ObjectFormat.Blob(content) =>
            s"blob ${content.size}\u0000${String(content.toArray, StandardCharsets.UTF_8)}".unsafeWrapped
        case ObjectFormat.Commit(treeId, parents) =>
            val treeLine = TREE_PREFIX
                .getBytes() ++ treeId.hex.unsafeWrapped :+ '\n'.toByte
            val parentLines = parents.flatMap: pid =>
                PARENT_PREFIX
                    .getBytes() ++ pid.hex.unsafeWrapped :+ '\n'.toByte
            val author_line = "author A <> 0 +0000".getBytes() :+ '\n'.toByte
            val content =
                ArraySeq.unsafeWrapArray(treeLine) ++ parentLines ++ author_line
            s"commit ${content.size}\u0000".unsafeWrapped ++ content

        case ObjectFormat.Tree(children) =>
            val content = children
                .flatMap: e =>
                    e.mode.getBytes() ++ (' '.toByte +: e.path
                        .getBytes()) ++ ('\u0000'.toByte +: e.id.bytes)
            s"tree ${content.size}\u0000".unsafeWrapped ++ content

    def decode(bytes: Seq[Byte]): ObjectFormat =
        if bytes.startsWith(BLOB_PREFIX) then
            ObjectFormat.Blob(extractContent(bytes, BLOB_PREFIX))
        else if bytes.startsWith(COMMIT_PREFIX) then
            parseCommit(extractContent(bytes, COMMIT_PREFIX))
        else if bytes.startsWith(TREE_PREFIX) then
            parseTree(extractContent(bytes, TREE_PREFIX))
        else ???

    @annotation.nowarn("msg=unused")
    private def parseCommit(content: Seq[Byte]): ObjectFormat.Commit =
        val parents = ArrayBuffer[Sha1Id]()
        require(content.startsWith(TREE_PREFIX))
        var index = TREE_PREFIX.length()
        val treeId = Sha1.ofHex(content.slice(index, index + 40))
        index = index + 41 // skip '\n'
        while content.slice(index, content.size).startsWith(PARENT_PREFIX) do
            index += PARENT_PREFIX.length()
            parents.addOne(Sha1.ofHex(content.slice(index, index + 40)))
            index += 41
        require(
          content.slice(index, content.size).startsWith(AUTHOR_PREFIX),
          String(content.slice(index, content.size).toArray)
        )
        index += AUTHOR_PREFIX.length()

        val nameStart = index
        while content(index) -> content(index + 1) != ' '.toByte -> '<'.toByte
        do index += 1
        val authorName = content.slice(nameStart, index)

        val mailStart = index + 2
        index = mailStart
        while content(index) != '>' do index += 1
        val mail = content.slice(mailStart, index)

        val epochStart = index + 2
        index = epochStart
        while content(index) != ' '.toByte do index += 1
        val epoch = String(content.slice(epochStart, index).toArray).toLong

        val tzStart = index + 1
        while content(index) != '\n' do index += 1
        val tz = content.slice(tzStart, index)
        ObjectFormat.Commit(treeId, parents.toSeq)

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
