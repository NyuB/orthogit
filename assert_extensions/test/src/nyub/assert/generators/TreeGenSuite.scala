package nyub.assert.generators

import nyub.assert.AssertExtensions
import TreeGenCommand.{Close, Leaf, Open}

class TreeGenSuite extends munit.FunSuite with AssertExtensions:
    val MutableMap = scala.collection.mutable.Map
    test("Empty command list return empty tree"):
        TreeGen.fromCommands(Seq()) isEqualTo node()

    test("One single close command returns empty tree"):
        TreeGen.fromCommands(Seq(Close())) isEqualTo node()

    test("One leaf command return a tree with one leaf child"):
        TreeGen.fromCommands(
          Seq(Leaf("edge", 1))
        ) isEqualTo node("edge" -> leaf(1))

    test("One open command returns one empty nested tree"):
        TreeGen.fromCommands(
          Seq(Open("edge"))
        ) isEqualTo node("edge" -> node())

    test("open, leaf, close, leaf"):
        TreeGen.fromCommands(
          Seq(Open("A"), Leaf("a", 1), Close(), Leaf("b", 2))
        ) isEqualTo node("A" -> node("a" -> leaf(1)), "b" -> leaf(2))

    private def node[E, L](prs: (E, TreeGen[E, L])*): TreeGenNode[E, L] =
        TreeGenNode(MutableMap(prs*))

    private def leaf[E, L](v: L): TreeGenLeaf[E, L] = TreeGenLeaf(v)

end TreeGenSuite
