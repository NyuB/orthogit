package nyub.assert

import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.util.Pretty
import org.scalacheck.Prop.forAll

trait PropertiesExtensions extends munit.Assertions:
    extension [A, B](f: A => B)
        infix def isTheInverseOf(g: B => A)(using
            p: Unit => Prop,
            arbitrary: Arbitrary[B],
            shrink: Shrink[B],
            prettyPrint: B => Pretty
        ): Prop =
            forAll: (b: B) =>
                assertEquals(f(g(b)), b)
