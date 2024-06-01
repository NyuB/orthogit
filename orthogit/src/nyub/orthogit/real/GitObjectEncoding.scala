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
        val index = ParsingIndex(content)
        index.skipPrefix(TREE_PREFIX)
        val treeId = index.readHexSha1()
        index.skipMarker('\n')

        while index.startsWith(PARENT_PREFIX) do
            index.skipPrefix(PARENT_PREFIX)
            parents.addOne(index.readHexSha1())
            index.skipMarker('\n')

        index.skipPrefix(AUTHOR_PREFIX)

        val nameStart = index
        val authorName = index.readUntilMarker(' ', '<')
        val mail = index.readUntilMarker('>', ' ')
        val epoch = String(index.readUntilMarker(' ').toArray).toLong
        val tz = index.readUntilMarker('\n')
        ObjectFormat.Commit(treeId, parents.toSeq)

    private def parseTree(content: Seq[Byte]): ObjectFormat.Tree =
        val entries = ArrayBuffer[TreeEntry]()
        val index = ParsingIndex(content)
        while !index.over do
            val mode = index.readUntilMarker(' ').utf8
            val path = index.readUntilMarker('\u0000').utf8
            val sha1 = index.readByteSha1()
            entries.addOne(
              TreeEntry(
                mode,
                path,
                sha1
              )
            )
        ObjectFormat.Tree(entries.toSeq)

    private class ParsingIndex(private val content: Seq[Byte]):
        private var index = 0

        def over = index >= content.size

        def startsWith(s: String): Boolean = startsWith(s.unsafeWrapped)
        def startsWith(bytes: Seq[Byte]): Boolean =
            content.slice(index, index + bytes.size) == bytes

        def readByteSha1(): Sha1Id =
            val res = content.slice(index, index + 20)
            index += 20
            Sha1.ofBytes(res)

        def readHexSha1(): Sha1Id =
            val res = content.slice(index, index + 40)
            index += 40
            Sha1.ofHex(res)

        def skipPrefix(s: String): Unit = skipPrefix(s.unsafeWrapped)
        def skipPrefix(prefix: Seq[Byte]): Unit =
            val here = content.slice(index, index + prefix.size)
            require(
              here == prefix,
              s"Expected current bytes to start with ${prefix} but was ${here}"
            )
            index += prefix.size

        def skipMarker(marker: Char): Unit = skipMarker(marker.toByte)
        def skipMarker(marker: Byte): Unit =
            val here = content(index)
            require(
              here == marker,
              s"Expected current byte to be ${marker} but got ${here}"
            )
            index += 1 // skip marker

        def readUntilMarker(a: Char, b: Char): Seq[Byte] =
            readUntilMarker(a.toByte, b.toByte)

        def readUntilMarker(a: Byte, b: Byte): Seq[Byte] =
            val start = index
            while content(index) -> content(index + 1) != a.toByte -> b.toByte
            do index += 1
            val res = content.slice(start, index)
            index += 2 // skip marker
            res

        def readUntilMarker(marker: Char): Seq[Byte] = readUntilMarker(
          marker.toByte
        )

        def readUntilMarker(marker: Byte): Seq[Byte] =
            val start = index
            while content(index) != marker do index += 1
            val res = content.slice(start, index)
            index += 1
            res

    private def extractContent(bytes: Seq[Byte], prefix: String): Seq[Byte] =
        val lstart = prefix.length()
        var lend = lstart
        while bytes(lend) != '\u0000' do lend += 1
        val len = String(bytes.slice(lstart, lend).toArray).toInt
        bytes.slice(lend + 1, lend + 1 + len)

    extension (s: String)
        private def unsafeWrapped = ArraySeq.unsafeWrapArray(s.getBytes())

    extension (bytes: Seq[Byte])
        private def utf8 = String(bytes.toArray, StandardCharsets.UTF_8)

end GitObjectEncoding
