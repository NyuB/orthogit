package nyub.orthogit.id

import nyub.assert.AssertExtensions
import scala.collection.immutable.ArraySeq

class Sha1Suite extends munit.FunSuite with AssertExtensions:
    val stringSha1 = Sha1.deriveSha1Identifier[String]

    test("Empty string hash"):
        stringSha1
            .id("")
            .hex isEqualTo "da39a3ee5e6b4b0d3255bfef95601890afd80709"

    test("Git object doc"):
        stringSha1
            .id("blob 16\u0000what is up, doc?")
            .hex isEqualTo "bd9dbf5aae1a3862dd1526723246b20206e5fc37"

    test("ofHex(hex) is identity"):
        val id = stringSha1.id("Blob")
        Sha1.ofHex(id.hex) isEqualTo id

    test("(implementation detail) ArraySeq equality"):
        ArraySeq.unsafeWrapArray(Array(1, 2, 3)) isEqualTo ArraySeq
            .unsafeWrapArray(Array(1, 2, 3))

end Sha1Suite
