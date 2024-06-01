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
    private val COMMITTER_PREFIX = "committer "
    private val SPACE = ' '
    private val NUL = '\u0000'
    private val LF = '\n'

    enum GitObject derives CanEqual:
        case Blob(content: Seq[Byte])
        case Commit(
            treeId: Sha1Id,
            parents: Seq[Sha1Id],
            author: CommitterInfo,
            committer: CommitterInfo,
            message: String
        )

        case Tree(children: Seq[TreeEntry])

    case class TreeEntry(val mode: String, val path: String, val id: Sha1Id)
    case class CommitterInfo(
        val name: String,
        val mail: String,
        val timestamp: Long,
        val timezone: String
    ):
        private[GitObjectEncoding] def encoded(prefix: String): Seq[Byte] =
            s"${prefix}${name} <${mail}> ${timestamp} ${timezone}".unsafeWrapped :+ LF.toByte

    def encode(obj: GitObject): Seq[Byte] = obj match
        case GitObject.Blob(content) =>
            s"${BLOB_PREFIX}${content.size}${NUL}${content.utf8}".unsafeWrapped
        case GitObject.Commit(treeId, parents, author, committer, message) =>
            val treeLine = TREE_PREFIX
                .getBytes() ++ treeId.hex.unsafeWrapped :+ LF.toByte
            val parentLines = parents.flatMap: pid =>
                PARENT_PREFIX
                    .getBytes() ++ pid.hex.unsafeWrapped :+ LF.toByte
            val authorLine = author.encoded(AUTHOR_PREFIX)
            val commiterLine = committer.encoded(COMMITTER_PREFIX)
            val content =
                ArraySeq.unsafeWrapArray(
                  treeLine
                ) ++ parentLines ++ authorLine ++ commiterLine ++ (LF.toByte +: ArraySeq
                    .unsafeWrapArray(message.getBytes()))
            s"${COMMIT_PREFIX}${content.size}${NUL}".unsafeWrapped ++ content

        case GitObject.Tree(children) =>
            val content = children
                .flatMap: e =>
                    e.mode.getBytes() ++ (SPACE.toByte +: e.path
                        .getBytes()) ++ (NUL.toByte +: e.id.bytes)
            s"${TREE_PREFIX}${content.size}${NUL}".unsafeWrapped ++ content

    def decode(bytes: Seq[Byte]): GitObject =
        if bytes.startsWith(BLOB_PREFIX) then
            GitObject.Blob(extractContent(bytes, BLOB_PREFIX))
        else if bytes.startsWith(COMMIT_PREFIX) then
            parseCommit(extractContent(bytes, COMMIT_PREFIX))
        else if bytes.startsWith(TREE_PREFIX) then
            parseTree(extractContent(bytes, TREE_PREFIX))
        else ???

    private def parseCommit(content: Seq[Byte]): GitObject.Commit =
        val parents = ArrayBuffer[Sha1Id]()
        val index = ParsingIndex(content)
        index.skipPrefix(TREE_PREFIX)
        val treeId = index.readHexSha1()
        index.skipMarker(LF)

        while index.startsWith(PARENT_PREFIX) do
            index.skipPrefix(PARENT_PREFIX)
            parents.addOne(index.readHexSha1())
            index.skipMarker(LF)

        val author = index.readAuthor()
        val commiter = index.readCommiter()
        index.skipMarker(LF)
        val message = index.readRemaining().utf8
        GitObject.Commit(treeId, parents.toSeq, author, commiter, message)

    private def parseTree(content: Seq[Byte]): GitObject.Tree =
        val entries = ArrayBuffer[TreeEntry]()
        val index = ParsingIndex(content)
        while !index.over do
            val mode = index.readUntilMarker(SPACE).utf8
            val path = index.readUntilMarker(NUL).utf8
            val sha1 = index.readByteSha1()
            entries.addOne(
              TreeEntry(
                mode,
                path,
                sha1
              )
            )
        GitObject.Tree(entries.toSeq)

    private class ParsingIndex(private val content: Seq[Byte]):
        private var index = 0

        def over = index >= content.size

        def readRemaining(): Seq[Byte] =
            val res = content.slice(index, content.size)
            index = content.size
            res

        def readAuthor(): CommitterInfo =
            skipPrefix(AUTHOR_PREFIX)
            readCommitterInfo()

        def readCommiter(): CommitterInfo =
            skipPrefix(COMMITTER_PREFIX)
            readCommitterInfo()

        private def readCommitterInfo(): CommitterInfo =
            val name = readUntilMarker(SPACE, '<')
            val mail = readUntilMarker('>', SPACE)
            val epoch = String(readUntilMarker(SPACE).toArray).toLong
            val tz = readUntilMarker(LF)
            CommitterInfo(name.utf8, mail.utf8, epoch, tz.utf8)

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
              s"Expected current byte to be '${marker.toChar}' but got '${here.toChar}'"
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
        while bytes(lend) != NUL do lend += 1
        val len = String(bytes.slice(lstart, lend).toArray).toInt
        bytes.slice(lend + 1, lend + 1 + len)

    extension (s: String)
        private def unsafeWrapped = ArraySeq.unsafeWrapArray(s.getBytes())

    extension (bytes: Seq[Byte])
        private def utf8 = String(bytes.toArray, StandardCharsets.UTF_8)

end GitObjectEncoding
