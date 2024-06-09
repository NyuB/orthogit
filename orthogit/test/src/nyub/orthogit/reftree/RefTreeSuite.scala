package nyub.orthogit.reftree

import nyub.assert.AssertExtensions

class RefTreeSuite extends munit.FunSuite with AssertExtensions:
    test("Expanded empty node is still the same"):
        ValueNode(Map.empty)
            .expand(identity, _ => Map.empty) isEqualTo ValueNode(Map.empty)

end RefTreeSuite
