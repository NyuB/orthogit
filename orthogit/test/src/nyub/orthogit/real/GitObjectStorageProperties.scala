package nyub.orthogit.real

import nyub.orthogit.storage.ObjectStorageProperties
import nyub.orthogit.id.Sha1.Sha1Id
import nyub.orthogit.git.StoredObjects
import nyub.orthogit.storage.ObjectStorage
import java.nio.file.Files
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.collection.immutable.ArraySeq
import nyub.orthogit.id.Sha1
import nyub.orthogit.id.Sha1Properties

type Obj = StoredObjects[Seq[Byte], Sha1Id, String, GitCommitMetadata]
given CanEqual[Obj, Obj] = CanEqual.derived
class GitObjectStorageProperties extends ObjectStorageProperties[Obj, Sha1Id]:
    override def scalaCheckInitialSeed =
        "IZCxaSKv8LuIyCoyP9U1vQv1JoUmL16MkY0kZRovMiG="

    override def emptyObjectStorage(): ObjectStorage[Obj, Sha1Id] =
        val tempDir = Files.createTempDirectory(".testgit")
        GitObjectStorage(tempDir)

    override def objectGenerator: Gen[Obj] =
        Gen.oneOf(storedObjectGen, storedTreeGen, storedCommitGen)

    private def storedObjectGen: Gen[StoredObjects.Blob[Seq[Byte]]] =
        byteGen.map(StoredObjects.Blob(_))

    private def storedTreeGen: Gen[StoredObjects.Tree[Sha1Id, String]] =
        Gen.mapOf(childGen).map(StoredObjects.Tree(_))

    private def storedCommitGen
        : Gen[StoredObjects.Commit[Sha1Id, GitCommitMetadata]] =
        Gen.listOf(Sha1Properties.sha1Gen)
            .flatMap: parentIds =>
                Sha1Properties.sha1Gen.flatMap: treeId =>
                    gitCommitMetadataGen.flatMap: meta =>
                        StoredObjects.Commit(parentIds, treeId, meta)

    private def childGen =
        Sha1Properties.sha1Gen.flatMap(id => pathGen.map(_ -> id))

    private def byteGen: Gen[Seq[Byte]] = Gen
        .stringOf(summon[Arbitrary[Char]].arbitrary)
        .map(_.getBytes())
        .map(ArraySeq.unsafeWrapArray)

    private def pathGen: Gen[String] = Gen.nonEmptyStringOf(Gen.alphaChar)

end GitObjectStorageProperties
