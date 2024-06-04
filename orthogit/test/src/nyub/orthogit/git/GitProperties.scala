package nyub.orthogit.git

import nyub.assert.AssertExtensions
import nyub.assert.PropertiesExtensions
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import nyub.assert.generators.{TreeGen, TreeGenLeaf, TreeGenNode}

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

        git.getBlob isTheInverseOf git.writeBlob

    property("getTree is the inverse of writeTree"):
        given Arbitrary[TreeGenNode[String, String]] =
            Arbitrary(TreeGen.treeGen)
        forAll: (t: TreeGenNode[String, String]) =>
            val git = TestGit()
            val tree = git.ofTreeGen(t)
            val treeId = git.writeTree(tree)
            val back = git.getTree(treeId)
            git.materializeTree(back) isEqualTo tree

    property("getCommit is the inverse of writeCommit"):
        given Arbitrary[TreeGenNode[String, String]] =
            Arbitrary(TreeGen.treeGen)
        forAll:
            (
                t1: TreeGenNode[String, String],
                t2: TreeGenNode[String, String],
                msg1: String,
                msg2: String
            ) =>
                val git = TestGit()
                val firstTree = git.ofTreeGen(t1)
                val firstTreeId = git.writeTree(firstTree)

                val secondTree = git.ofTreeGen(t2)
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

    extension (git: TestGit)
        private def materializeTree(tree: git.Tree): git.Tree =
            val materializedChildren = tree.children.view
                .mapValues:
                    case b: git.Blob     => b
                    case git.BlobRef(id) => git.getBlob(id)
                    case t: git.Tree     => git.materializeTree(t)
                    case git.TreeRef(id) => git.materializeTree(git.getTree(id))
                .toMap
            git.Tree(materializedChildren)

        private def ofTreeGen(gen: TreeGenNode[String, String]): git.Tree =
            val children = gen.children.view
                .mapValues:
                    case t: TreeGenNode[String, String] => git.ofTreeGen(t)
                    case TreeGenLeaf(value)             => git.Blob(value)
                .toMap
            git.Tree(children)

end GitProperties
