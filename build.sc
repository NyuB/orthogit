import mill._, scalalib._

object Versions {
    val scala = "3.4.1"
    val munit = "1.0.0"
}

trait SharedConfiguration extends ScalaModule {
    override def scalaVersion: T[String] = Versions.scala
    override def scalacOptions: T[Seq[String]] =
        Seq(
          "-deprecation",
          "-language:strictEquality",
          "-Werror",
          "-Wimplausible-patterns",
          "-Wnonunit-statement",
          "-WunstableInlineAccessors",
          "-Wunused:all",
          "-Wvalue-discard",
          "-Xlint:all",
          "-Ysafe-init"
        )

    trait Tests extends ScalaTests with TestModule.Munit {
        override def ivyDeps = super.ivyDeps() ++ Agg(
          ivy"org.scalameta::munit:${Versions.munit}",
          ivy"org.scalameta::munit-scalacheck:${Versions.munit}"
        )

        override def moduleDeps = super.moduleDeps ++ Seq(assert_extensions)
    }

}

object assert_extensions extends ScalaModule with SharedConfiguration {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalameta::munit:${Versions.munit}",
      ivy"org.scalameta::munit-scalacheck:${Versions.munit}"
    )

    object test extends Tests
}

object orthogit extends ScalaModule with SharedConfiguration {
    object test extends Tests
}
