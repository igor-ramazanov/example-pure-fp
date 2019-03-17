name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "1.2.0",
  "com.propensive" %% "magnolia" % "0.10.0",
  "io.circe" %% "circe-core" % "0.11.1",
  "io.circe" %% "circe-generic" % "0.11.1",
  "io.circe" %% "circe-parser" % "0.11.1",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.github.mpilquist" %% "simulacrum" % "0.15.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")