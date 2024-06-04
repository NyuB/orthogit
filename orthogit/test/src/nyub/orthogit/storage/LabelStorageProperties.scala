package nyub.orthogit.storage

import nyub.assert.AssertExtensions
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait LabelStorageProperties[Label, Obj](using CanEqual[Obj, Obj])
    extends munit.ScalaCheckSuite
    with AssertExtensions:
    def emptyLabelStorage(): LabelStorage[Label, Obj]
    def objectGenerator: Gen[Obj]
    def labelGenerator: Gen[Label]

    private given Arbitrary[Obj] = Arbitrary(objectGenerator)
    private given Arbitrary[Label] = Arbitrary(labelGenerator)

    property("get is the inverse of set"):
        forAll: (l: Label, o: Obj) =>
            val storage = emptyLabelStorage()
            storage.set(l, o)
            storage.get(l) isEqualTo Some(o)

    property("after delete get returns None"):
        forAll: (l: Label, o: Obj) =>
            val storage = emptyLabelStorage()
            storage.set(l, o)
            storage.delete(l) isEqualTo Some(o)
            storage.get(l) isEqualTo None

end LabelStorageProperties

class InMemoryLabelStorageProperties extends LabelStorageProperties[Long, Int]:
    override def emptyLabelStorage(): LabelStorage[Long, Int] =
        LabelStorage.InMemory()

    override def labelGenerator: Gen[Long] = summon[Arbitrary[Long]].arbitrary
    override def objectGenerator: Gen[Int] = summon[Arbitrary[Int]].arbitrary
end InMemoryLabelStorageProperties
