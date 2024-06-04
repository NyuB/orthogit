package nyub.orthogit.storage

import nyub.assert.AssertExtensions
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.orthogit.id.Sha1.deriveSha1Identifier
import nyub.orthogit.id.Identifier

trait ObjectStorageProperties[Obj, Id](using
    CanEqual[Obj, Obj],
    CanEqual[Id, Id]
) extends munit.ScalaCheckSuite
    with AssertExtensions:
    def emptyObjectStorage(): ObjectStorage[Obj, Id]
    def objectGenerator: Gen[Obj]

    private given Arbitrary[Obj] = Arbitrary(objectGenerator)

    property("get is the inverse of store"):
        forAll: (o: Obj) =>
            val storage = emptyObjectStorage()
            val id = storage.store(o)
            storage.get(id) isEqualTo Some(o)

    property(
      "two objects are equal if and only if they are stored with the same id"
    ):
        forAll: (objPair: (Obj, Obj)) =>
            val (a, b) = objPair
            val emptyStorage = emptyObjectStorage()
            val idA = emptyStorage.store(a)
            val idB = emptyStorage.store(b)

            if a == b then idA isEqualTo idB else idA isNotEqualTo idB

end ObjectStorageProperties

class InMemorySha1ObjectStorageProperties
    extends ObjectStorageProperties[String, Sha1Id]:
    given stringSha1Identifier: Identifier[String, Sha1Id] =
        deriveSha1Identifier[String]

    override def emptyObjectStorage(): ObjectStorage[String, Sha1Id] =
        ObjectStorage.InMemory()

    override def objectGenerator: Gen[String] =
        summon[Arbitrary[String]].arbitrary

end InMemorySha1ObjectStorageProperties
