package nyub.orthogit.id

import scala.collection.immutable.ArraySeq
import java.math.BigInteger

object Sha1:
    opaque type Sha1Id = Seq[Byte]
    given CanEqual[Sha1Id, Sha1Id] = CanEqual.derived
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
