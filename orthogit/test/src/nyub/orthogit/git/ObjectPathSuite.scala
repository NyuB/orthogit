package nyub.orthogit.git

import nyub.assert.AssertExtensions

class ObjectPathSuite extends munit.FunSuite with AssertExtensions:
    test("Single name path contains itself"):
        ObjectPath("a.txt").contains(ObjectPath("a.txt"))

    test("a/b/c contains a/b"):
        ObjectPath(Seq("a", "b"), "c").contains(
          ObjectPath(Seq("a"), "b")
        ) isEqualTo true

    test("a/b/c does not contains b/c"):
        ObjectPath(Seq("a", "b"), "c").contains(
          ObjectPath(Seq("b"), "c")
        ) isEqualTo false

    test("a/b contains a"):
        ObjectPath(Seq("a"), "b").contains(
          ObjectPath(Seq.empty, "a")
        ) isEqualTo true

end ObjectPathSuite
