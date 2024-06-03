package nyub.orthogit.git

import nyub.assert.AssertExtensions
import nyub.assert.PropertiesExtensions
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

    enum TreeGenCommand[Edge, Leaf]:
        case MakeLeaf(edge: Edge, leaf: Leaf)
        case MakeEdge(edge: Edge)

    sealed trait TreeGen[Edge, Leaf]
    case class TreeGenNode[Edge, Leaf](
        val children: scala.collection.mutable.Map[Edge, TreeGen[Edge, Leaf]]
    ) extends TreeGen[Edge, Leaf]

    case class TreeGenLeaf[Edge, Leaf](val value: Leaf)
        extends TreeGen[Edge, Leaf]

    object TreeGen:
        def treeGen[Edge, Leaf](using
            edgeGen: Arbitrary[Edge],
            leafGen: Arbitrary[Leaf]
        ): Gen[TreeGenNode[Edge, Leaf]] =
            val commands = Gen.frequency(
              5 -> leafCommandGen[Edge, Leaf],
              1 -> edgeCommandGen[Edge, Leaf]
            )
            Gen.listOf(commands).map(TreeGen.fromCommands)

        def leafCommandGen[Edge, Leaf](using
            edgeGen: Arbitrary[Edge],
            leafGen: Arbitrary[Leaf]
        ): Gen[TreeGenCommand[Edge, Leaf]] =
            edgeGen.arbitrary.flatMap: e =>
                leafGen.arbitrary.map: l =>
                    TreeGenCommand.MakeLeaf(e, l)

        def edgeCommandGen[Edge, Leaf](using
            edgeGen: Arbitrary[Edge]
        ): Gen[TreeGenCommand[Edge, Leaf]] =
            edgeGen.arbitrary.map: e =>
                TreeGenCommand.MakeEdge(e)

        def empty[Edge, Leaf](): TreeGenNode[Edge, Leaf] = TreeGenNode(
          scala.collection.mutable.Map.empty
        )

        def fromCommands[Edge, Leaf](
            commands: Iterable[TreeGenCommand[Edge, Leaf]]
        ): TreeGenNode[Edge, Leaf] =
            val start = empty[Edge, Leaf]()
            commands
                .foldLeft(TreeGenCursor(start, start)): (acc, command) =>
                    command match
                        case TreeGenCommand.MakeLeaf(edge, leaf) =>
                            acc.leaf(edge, leaf)
                        case TreeGenCommand.MakeEdge(edge) => acc.node(edge)
                .root

    class TreeGenCursor[Edge, Leaf](
        val root: TreeGenNode[Edge, Leaf],
        var cursor: TreeGenNode[Edge, Leaf]
    ):
        type Self = TreeGenCursor[Edge, Leaf]
        def leaf(edge: Edge, value: Leaf): Self =
            this.cursor.children.update(edge, TreeGenLeaf(value))
            this

        def node(edge: Edge): Self =
            val next = this.cursor.children.getOrElseUpdate(
              edge,
              TreeGenNode[Edge, Leaf](scala.collection.mutable.Map.empty)
            )
            next match
                case t: TreeGenNode[Edge, Leaf] => TreeGenCursor(root, t)
                case l: TreeGenLeaf[Edge, Leaf] => this

end GitProperties
