// Formats the sources; CI checks the formatting with scalafmtCheckAll.
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")

// Publishes to Maven Central from GitHub Actions and derives the version from git tags.
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.1")
