package nyub.orthogit.reftree

import nyub.assert.AssertExtensions

class RefTreeSuite extends munit.FunSuite with AssertExtensions:
    test("Expanded empty node is still the same"):
        ValueNode(Map.empty)
            .expand(identity, _ => Map.empty) isEqualTo ValueNode(Map.empty)

    test("expand"):
        def expandLeaf(id: String) = id.toLong
        def expandNode(id: String) = Map(id -> RefLeaf("42"))
        val tree = ValueNode(
          Map(
            "A" -> ValueLeaf(0L),
            "B" -> ValueNode(Map("C" -> RefLeaf("1"))),
            "D" -> RefNode("E")
          )
        )
        tree.expand(expandLeaf, expandNode) isEqualTo ValueNode(
          Map(
            "A" -> ValueLeaf(0L),
            "B" -> ValueNode(Map("C" -> ValueLeaf(1L))),
            "D" -> ValueNode(Map("E" -> ValueLeaf(42L)))
          )
        )

    test("compress"):
        def compressLeaf(c: Char): String = s"(${c.toString()})"
        def compressNode(map: Map[String, Ref[String, String]]): String =
            map.map: (k, v) =>
                v match
                    case RefNode(id) => s"${k} -> ${id}"
                    case RefLeaf(id) => s"${k} -> ${id}"
            .mkString("[", " | ", "]")

        val tree = ValueNode(
          Map(
            "A" -> ValueLeaf('a'),
            "B" -> ValueNode(Map("C" -> RefLeaf("(b)")))
          )
        )
        tree.compress(
          compressLeaf,
          compressNode
        ) isEqualTo "[A -> (a) | B -> [C -> (b)]]"

    test("insert leaf at root"):
        def noExpansionRequired(any: Any) = throw IllegalStateException(
          s"No expansion expected, cannot expand ${any}"
        )
        val tree = ValueNode(
          Map.empty
        )
        tree.insert(
          "A",
          Seq.empty,
          ValueLeaf(0L),
          noExpansionRequired
        ) isEqualTo ValueNode(
          Map("A" -> ValueLeaf(0L))
        )

    test("insert leaf at existing path"):
        def noExpansionRequired(any: Any) = throw IllegalStateException(
          s"No expansion expected, cannot expand ${any}"
        )
        val tree = ValueNode(
          Map(
            "A" -> ValueNode(Map("B" -> RefLeaf("b")))
          )
        )
        tree.insert(
          "B",
          Seq("A"),
          RefLeaf("c"),
          noExpansionRequired
        ) isEqualTo ValueNode(
          Map("A" -> ValueNode(Map("B" -> RefLeaf("c"))))
        )

    test("insert leaf requiring a node expansion"):
        def expandNode(id: String) = Map(id -> RefLeaf("B"))
        val tree = ValueNode(
          Map(
            "A" -> RefNode("0")
          )
        )
        tree.insert(
          "C",
          Seq("A"),
          ValueLeaf(42),
          expandNode
        ) isEqualTo ValueNode(
          Map("A" -> ValueNode(Map("0" -> RefLeaf("B"), "C" -> ValueLeaf(42))))
        )

end RefTreeSuite
