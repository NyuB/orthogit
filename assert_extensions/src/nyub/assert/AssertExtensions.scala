package nyub.assert

import java.nio.file.{Files, Path}
trait AssertExtensions extends munit.Assertions:
    extension [T](t: T)
        infix def isEqualTo[U <: T](other: U)(using CanEqual[T, U]): Unit =
            assertEquals(t, other)

        infix def matches(pf: PartialFunction[T, Boolean]): Unit =
            if pf.isDefinedAt(t) then assertEquals(pf(t), true)
            else fail(s"Expected ${t} to match ${pf}")

    extension [T](l: Iterable[T])
        infix def containsExactlyInAnyOrder(expected: Iterable[T])(using
            CanEqual[T, T]
        ): Unit =
            assertEquals(
              l.size,
              expected.size,
              s"Expected to contains exactly the elements of ${expected} but sizes do not match (actual=${l.size} expected=${expected.size})"
            )
            expected.foreach(item =>
                assertEquals(
                  l.exists(_ == item),
                  true,
                  s"actual ${l} does not contain expected item ${item} from ${expected}"
                )
            )

    extension (s: String)
        infix def isEqualToLines(lines: Iterable[String]): Unit =
            assertEquals(s, lines.mkString("\n"))

    extension (p: Path)
        infix def hasContent(content: String): Unit =
            assertEquals(p.toFile().isFile(), true, s"$p is not a file")
            Files.readString(p) isEqualTo content

        infix def hasContent(content: Iterable[String]): Unit =
            p hasContent content.mkString("\n")
