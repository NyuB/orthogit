package nyub.orthogit.id

import scala.collection.immutable.ArraySeq
import java.math.BigInteger

object Sha1:
    opaque type Sha1Id = Seq[Byte]
    given CanEqual[Sha1Id, Sha1Id] = CanEqual.derived
    def ofHex(hexRepr: String): Sha1Id =
        if hexRepr.length() != 40 then
            throw IllegalArgumentException(
              s"the string '${hexRepr}' is not a valid SHA-1 hexadecimal representation, expected 40 characters got ${hexRepr.length()}"
            )
        hexRepr.foreach(c =>
            require(
              "ABCDEFabcdef0123456789".contains(c),
              s"'${hexRepr}' is not a valid SHA-1 hexadecimal representation, '${c}' is not a valid hexadecimal character"
            )
        )

        val res = Array.ofDim[Byte](20)
        for i <- 0 until 20 do
            val left = hexRepr.charAt(2 * i).hexToInt << 4
            val right = hexRepr.charAt(2 * i + 1).hexToInt
            res(i) = (left | right).toByte

        ArraySeq.unsafeWrapArray(res)

    extension (c: Char)
        private def hexToInt: Int = c.toLower match
            case 'a'   => 10
            case 'b'   => 11
            case 'c'   => 12
            case 'd'   => 13
            case 'e'   => 14
            case 'f'   => 15
            case digit => digit.asDigit

    extension (sha1: Sha1Id)
        def hex: String = String.format("%040x", BigInteger(1, sha1.toArray))

    object Sha1Identifier extends Identifier[Array[Byte], Sha1Id]:
        override def id(obj: Array[Byte]): Sha1Id =
            ArraySeq.unsafeWrapArray(init.digest(obj))

        private def init = java.security.MessageDigest.getInstance("SHA1")

    trait ByteIdentity[Obj]:
        def id(obj: Obj): Array[Byte]

    given ByteIdentity[String] with
        override def id(obj: String): Array[Byte] = obj.getBytes()

    given deriveSha1Identifier[Obj](using
        str: ByteIdentity[Obj]
    ): Identifier[Obj, Sha1Id] with
        override def id(obj: Obj): Sha1Id = Sha1Identifier.id(str.id(obj))
