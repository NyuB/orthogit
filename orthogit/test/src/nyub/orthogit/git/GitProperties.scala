package nyub.orthogit.git

import nyub.assert.AssertExtensions
import nyub.assert.PropertiesExtensions
import nyub.assert.generators.{TreeGen, TreeGenLeaf, TreeGenNode}
import nyub.orthogit.reftree.{expand, ValueLeaf, ValueNode}
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class GitProperties
    extends munit.ScalaCheckSuite
    with AssertExtensions
    with PropertiesExtensions:

    override def scalaCheckInitialSeed =
        "PFZ3V5OnA9T6Nx1ClLNzXTtz3KPzyCz8Aa1keTZguVJ="

    property("getBlob is the inverse of writeBlob"):
        val git = TestGit()
        given Arbitrary[git.Blob] =
            Arbitrary(Gen.asciiPrintableStr.map(git.Blob(_)))

        // NB: all the writes are performed on the same mutable git instance
        git.getBlob isTheInverseOf git.writeBlob

    property("getTree is the inverse of writeTree"):
        forAll: (tree: ValueNode[Nothing, Nothing, String, String]) =>
            val git = TestGit()
            val treeId = git.writeTree(tree)
            val back = git.getTree(treeId)
            val materialized = git.materializeTree(back)
            materialized isEqualTo tree

    property("getCommit is the inverse of writeCommit"):
        forAll:
            (
                firstTree: ValueNode[Nothing, Nothing, String, String],
                secondTree: ValueNode[Nothing, Nothing, String, String],
                msg1: String,
                msg2: String
            ) =>
                val git = TestGit()

                val firstTreeId = git.writeTree(firstTree)
                val secondTreeId = git.writeTree(secondTree)

                val firstCommit = git.Commit(Seq.empty, firstTreeId, msg1)
                val firstCommitId = git.writeCommit(firstCommit)

                val secondCommit =
                    git.Commit(Seq(firstCommitId), secondTreeId, msg2)
                val secondCommitId = git.writeCommit(secondCommit)

                val firstGet = git.getCommit(firstCommitId)
                val secondGet = git.getCommit(secondCommitId)

                firstGet isEqualTo firstCommit
                secondGet isEqualTo secondCommit

    given gitTreeGenerator
        : Arbitrary[ValueNode[Nothing, Nothing, String, String]] =
        Arbitrary(TreeGen.treeGen[String, String].map(gitTreeOfTreeGen))

    extension (git: TestGit)
        private def materializeTree(tree: git.Tree): git.Tree =
            tree.expand(
              id => git.getBlob(id).value,
              t => git.getTreeChildrenIds(t)
            )

        private def getTreeChildrenIds(
            id: git.TreeId
        ) =
            git.getTree(id)
                .children
                .view
                .mapValues:
                    case b: git.BlobRef => b
                    case t: git.TreeRef => t
                    case _ =>
                        throw IllegalStateException(
                          "Did not expect anything not stored yet"
                        )
                .toMap

    private def gitTreeOfTreeGen(
        gen: TreeGenNode[String, String]
    ): ValueNode[Nothing, Nothing, String, String] =
        val children = gen.children.view
            .mapValues:
                case t: TreeGenNode[String, String] => gitTreeOfTreeGen(t)
                case TreeGenLeaf(value)             => ValueLeaf(value)
            .toMap
        ValueNode(children)

end GitProperties
