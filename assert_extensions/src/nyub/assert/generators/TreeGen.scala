package nyub.assert.generators

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

/** Mutable tree structure for scalacheck genrators
  */
sealed trait TreeGen[Edge, Leaf]

case class TreeGenNode[Edge, Leaf](
    val children: scala.collection.mutable.Map[Edge, TreeGen[Edge, Leaf]]
) extends TreeGen[Edge, Leaf]
    derives CanEqual

case class TreeGenLeaf[Edge, Leaf](val value: Leaf) extends TreeGen[Edge, Leaf]
    derives CanEqual

/** Commands for [[TreeGen]] building. Applied relatively to a 'cursor'
  * positioned somewhere at a tree node.
  */
enum TreeGenCommand[E, L]:
    /** Add a leaf to the current node's children.
      *
      * @param edge
      *   the edge leading to the added leaf
      * @param leaf
      *   the leaf value
      */
    case Leaf(edge: E, leaf: L)

    /** Add a node to the current node's childen and move the cursor to this
      * node.
      *
      * @param edge
      */
    case Open(edge: E)

    /** Move up to the current node's parent. If the current node is the root of
      * the tree, has no effect.
      */
    case Close()

object TreeGen:
    /** Generates [[TreeGen]] data structures
      *
      * @param edgeGen
      *   arbitrary generator for the tree edges
      * @param leafGen
      *   arbitrary generator for the tree leaves
      * @return
      *   an arbitrary generator of [[TreeGen]] with arbitrary edges and leaves
      */
    def treeGen[Edge, Leaf](using
        edgeGen: Arbitrary[Edge],
        leafGen: Arbitrary[Leaf]
    ): Gen[TreeGenNode[Edge, Leaf]] =
        val commands = Gen.frequency(
          5 -> leafCommandGen[Edge, Leaf],
          2 -> openCommandGen[Edge, Leaf],
          3 -> closeCommandGen[Edge, Leaf] // since we can always open but sometimes not close, generate more close than open
        )
        Gen.listOf(commands).map(TreeGen.fromCommands)

    def leafCommandGen[Edge, Leaf](using
        edgeGen: Arbitrary[Edge],
        leafGen: Arbitrary[Leaf]
    ): Gen[TreeGenCommand[Edge, Leaf]] =
        edgeGen.arbitrary.flatMap: e =>
            leafGen.arbitrary.map: l =>
                TreeGenCommand.Leaf(e, l)

    def openCommandGen[Edge, Leaf](using
        edgeGen: Arbitrary[Edge]
    ): Gen[TreeGenCommand[Edge, Leaf]] =
        edgeGen.arbitrary.map(TreeGenCommand.Open(_))

    def closeCommandGen[Edge, Leaf](using
        edgeGen: Arbitrary[Edge]
    ): Gen[TreeGenCommand[Edge, Leaf]] =
        Gen.oneOf(Seq(TreeGenCommand.Close()))

    def empty[Edge, Leaf](): TreeGenNode[Edge, Leaf] = TreeGenNode(
      scala.collection.mutable.Map.empty
    )

    /** Builds a [[TreeGen]] by applying the `commands` sequence to an initially
      * empty tree
      *
      * @param commands
      *   the sequence of command to apply
      * @return
      *   a tree structure
      */
    def fromCommands[Edge, Leaf](
        commands: Iterable[TreeGenCommand[Edge, Leaf]]
    ): TreeGenNode[Edge, Leaf] =
        val start = empty[Edge, Leaf]()
        commands
            .foldLeft(TreeGenCursor(None, start)): (acc, command) =>
                command match
                    case TreeGenCommand.Leaf(edge, leaf) =>
                        acc.leaf(edge, leaf)
                    case TreeGenCommand.Open(edge) => acc.open(edge)
                    case TreeGenCommand.Close()    => acc.close()
            .root

private class TreeGenCursor[Edge, Leaf](
    val previous: Option[TreeGenCursor[Edge, Leaf]],
    val current: TreeGenNode[Edge, Leaf]
):
    type Self = TreeGenCursor[Edge, Leaf]
    def leaf(edge: Edge, value: Leaf): Self =
        this.current.children.updateWith(edge):
            case Some(v) => None
            case None    => Some(TreeGenLeaf[Edge, Leaf](value))
        : @annotation.nowarn("msg=unused value")
        this

    def open(edge: Edge): Self =
        val next = this.current.children.getOrElseUpdate(
          edge,
          TreeGenNode[Edge, Leaf](scala.collection.mutable.Map.empty)
        )
        next match
            case t: TreeGenNode[Edge, Leaf] => TreeGenCursor(Some(this), t)
            case l: TreeGenLeaf[Edge, Leaf] => this

    def close(): Self =
        this.previous.getOrElse(this)

    def root: TreeGenNode[Edge, Leaf] = this.previous match
        case None    => this.current
        case Some(c) => c.root
