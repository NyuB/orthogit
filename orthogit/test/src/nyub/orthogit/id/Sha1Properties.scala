package nyub.orthogit.id

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import nyub.assert.AssertExtensions

class Sha1Properties extends munit.ScalaCheckSuite with AssertExtensions:
    property("Unique identifier"):
        val stringSha1 = Sha1.deriveSha1Identifier[String]
        forAll: (s1: String, s2: String) =>
            val id1 = stringSha1.id(s1)
            val id2 = stringSha1.id(s2)
            if s1 == s2 then id1 isEqualTo id2
            else id1 isNotEqualTo id2

    property("hex is the inverse of fromHex"):
        given Arbitrary[String] =
            Arbitrary(Gen.stringOfN(40, Gen.hexChar))
        forAll: (hexString: String) =>
            Sha1.ofHex(hexString).hex isEqualTo hexString.toLowerCase()

end Sha1Properties
